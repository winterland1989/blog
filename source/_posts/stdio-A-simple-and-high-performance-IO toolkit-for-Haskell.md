---
title: "stdio - A simple and high-performance IO toolkit for Haskell"
date: 2019/2/17 20:39
tags: haskell
---

Yesterday I and my friend Tao He write a short [release message](https://mail.haskell.org/pipermail/haskell-cafe/2019-February/130706.html) at a local cafe, we released a [new IO library](http://hackage.haskell.org/package/stdio) for GHC based on our previous work on combining libuv and GHC. Here I'd like to spend some time explaining what's inside our new library besides the libuv binding part, what's the motivation and what's our solution. 

A new `Bytes` type
==================

`ByteString` is used as the packed bytes type for years, it's used as the buffer type in IO, serialization, parsing, etc. Because it uses pinned memory, thus allow everything works on `Ptr ()`s works on it. But this is really not ideal: pinned memory is only suitable for large blocks, if lots of small `ByteString`s are created during runtime, memory fragmentation may happen. And due to historic reason, `ByteString` choose to use `ForeignPtr` which contain a sum type, which is harder to unpack. To address these problems, in stdio we do a couple of things: 

+ To solve the system FFI use case, we add a separated `CBytes` type:

```haskell
data CBytes
    = CBytesOnHeap  {-# UNPACK #-} !(PrimArray Word8)   -- ^ On heap pinned 'PrimArray'
    | CBytesLiteral {-# UNPACK #-} !CString             -- ^ String literals with static address
```

Which always wrap an immutable null-terminated string, It's used as the file path type, domain name type, etc. We use a rewrite rule so that a literal `CBytes` is constructed directly from a literal `Addr#` in O(1).

+ We build a `ByteArray#` based vector type, which based on `PrimArray` in primitive package.

```
-- from primitive >= 0.6.4.0
data PrimArray a = PrimArray ByteArray#

data PrimVector a = PrimVector
    {-# UNPACK #-} !(PrimArray a)   -- ^ payload
    {-# UNPACK #-} !Int             -- ^ offset in elements of type a rather than in bytes
    {-# UNPACK #-} !Int             -- ^ length in elements of type a rather than in bytes
```

With this definition, GHC RTS can freely choose to allocate on nursery or pinned blocks depending on vector size, creating small vectors are much cheaper. When you want to pass this new vector to a safe FFI, you will have to check if it's a pinned one, if not a pinned copy is made (in unsafe FFI case, you can pass the `ByteArray#` directly).

+ Now makes `Bytes`an alias to `PrimVector Word8`.

There's a key function that `ByteString` beats vector: `break`, it uses `memchr` if possible, which is SIMD enabled. This is very important because a high performance `Parser` will need a fast `takeWhile (==x)`, which in turn rely on finding the first specific byte quickly. We leverage GHC rewrite rules to solve this problem: use `memchr` if types matches `Bytes`. 

Unified `Vector`
================

Like what we suggested in the [last blog post](/2017/08/18/an-unified-array-interface/), we build a unified vector for both boxed and unboxed vector. I didn't provide separated typeclasses for slicing, indexing, etc, instead, I made it very clear that a vector is just a slice of an array:

```haskell
class (Arr (MArray v) (IArray v) a) => Vec v a where
    type MArray v = (marr :: * -> * -> *) | marr -> v
    type IArray v = (iarr :: * -> *) | iarr -> v
    toArr :: v a -> (IArray v a, Int, Int)
    fromArr :: IArray v a -> Int -> Int -> v a
```

Both boxed `Vector` and unboxed `PrimVector` 's definition is as simple as it can be:

```haskell
data Vector a = Vector
    {-# UNPACK #-} !(SmallArray a)  -- ^ payload
    {-# UNPACK #-} !Int             -- ^ offset
    {-# UNPACK #-} !Int             -- ^ length

instance Vec Vector a where
    type MArray Vector = SmallMutableArray
    type IArray Vector = SmallArray
    toArr (Vector arr s l) = (arr, s, l)
    fromArr = Vector

data PrimVector a = PrimVector
    {-# UNPACK #-} !(PrimArray a)   -- ^ payload
    {-# UNPACK #-} !Int             -- ^ offset in elements of type a rather than in bytes
    {-# UNPACK #-} !Int             -- ^ length in elements of type a rather than in bytes

instance Prim a => Vec PrimVector a where
    type MArray PrimVector = MutablePrimArray
    type IArray PrimVector = PrimArray
    toArr (PrimVector arr s l) = (arr, s, l)
    fromArr = PrimVector
```

In stdio vector combinators all work on `Vec v a` constraint, so something like type changing `map`, `traverse`, `zipWith'`, etc are possible:

```haskell
-- We don't require result vector type is the same with input one 
map :: forall u v a b. (Vec u a, Vec v b) => (a -> b) -> u a -> v b
traverse :: (Vec v a, Vec u b, Applicative f) => (a -> f b) -> v a -> f (u b)
zipWith' :: (Vec v a, Vec u b, Vec w c) => (a -> b -> c) -> v a -> u b -> w c
```

I even implemented a KMP searching algorithm with this unified vector interface:

```haskell
indices :: (Vec v a, Eq a) => v a -> v a -> Bool -> [Int]
```

Now enjoy searching an `Int` vector or boxed vector with O(n + k) time complexity!

`Builder` and `Parser`
======================

Our new `Bytes` requires a whole new machinery of constrution and deconstrution, traditional `Builder` and `Parser`s all works on `Ptr ()`s, which is not suitable for our new `ByteArray#` based `Bytes` anymore. When we first want to design a solution for our new `Bytes` type at the time of 2017, we met a problem though: we can't do unaligned access on `ByteArray#`s, e.g. we can't read or write 4 bytes as `Word32` at a random index of a `ByteArray#` like a `Ptr ()`, the primitives GHC provides only allow us to read or write on aligned indexes. So the work on stdio stalled, until in GHC 8.6.1 [new prim-ops are added](https://phabricator.haskell.org/D4488): 

```haskell
indexWord8ArrayAsWord16# :: ByteArray# -> Int# -> Word#
indexWord8ArrayAsWord32# :: ByteArray# -> Int# -> Word#
indexWord8ArrayAsWord64# :: ByteArray# -> Int# -> Word#
...
```

This is very important because if we can't do unaligned access with `ByteArray#`, then we're forced to break down a primitive value into bytes during serializing, or constructing from several bytes when parsing, which will be much slower than old `Storable` way. In stdio we provide [`UnalignedAccess` type class](http://hackage.haskell.org/package/stdio-0.1.0.0/docs/Std-Data-PrimArray-UnalignedAccess.html) to leverage these new prim-ops, which probably should be picked up in the next version of `primitive` package.

Now the missing piece is ready, we can have our `Builder` and `Parser` now. In stdio we design something different: A `Builder` monad not only support building `Bytes` chunks, but also support building short strict `Bytes`. That is, we support different `AllocateStrategy`s:

```haskell
data AllocateStrategy s
    = DoubleBuffer                      -- Double the buffer and continue building
    | InsertChunk {-# UNPACK #-} !Int   -- Insert a new chunk and continue building
    | OneShotAction (V.Bytes -> ST s ())  -- Freeze current chunk and perform action with it.
```

This is an internal type to implement different allocate strategies, and the `Builder` monad use this information to handle writing across buffer boundaries. The final API exposed to the user is very nature:

```haskell
buildBytes :: Builder a -> V.Bytes
buildBytesList :: Builder a -> [V.Bytes]
buildAndRun :: (V.Bytes -> IO ()) -> Builder a -> IO ()
```

As for `Parser` type, we choose a very simple CPS formula:

```haskell
data Result a
    = Success !V.Bytes a
    | Failure !V.Bytes String
    | Partial (V.Bytes -> Result a)

type ParseStep r = V.Bytes -> Result r

newtype Parser a = Parser { runParser :: forall r .  (a -> ParseStep r) -> ParseStep r }
```

After I implemented this `Parser` then I found it's exactly the same one with [scanner](http://hackage.haskell.org/package/scanner) package, it's not a coincidence though: in my own benchmark this formula is as fast as the non-resumable one from store package!

Unlike what Yuras claimed in scanner package, it's actually [quite easy](https://github.com/haskell-stdio/stdio/blob/master/Std/Data/Parser/Base.hs#L116) to provide an `Alternative` instance for this formula. And if a user doesn't use it, there will be no backtracking cost at all. 

We provide high quality numeric `Builder`s and `Parser`s as well, the IEEE floating formatting is based on my [grusi3 patch](https://github.com/haskell/bytestring/pull/115), which is much faster.

Some extra words on `Builder` here: why do we choose a monadic interface instead of monoid one? The reasons in my mind are:

+ A monadic interface is more powerful, you can do something like accumulating a verification value while building bytes, or add an extra `ReaderT` layer to do some building constant injections.

+ A monadic interface is more symmetrical to `Parser`, e.g. we probably have something like

```haskell
import qualified Std.Data.Builder as B
import qualified Std.Data.Parser  as P

buildFoo :: Foo -> Builder ()
buildFoo (Foo x y z) = do
    B.bytes x
    B.encodePrim magic
    B.encodePrim y
    B.double z

parseFoo :: Parser Foo
parseFoo = do 
    x <- P.takeWhile (/=magic)
    P.word8 magic 
    y <- P.decodePrim 
    z <- P.double 
    return (Foo x y z)
```

+ A monadic interface works extremely well with `Control.Monad`, while control structures on monoid is a bit of lacking.

Buffered IO
===========

Buffered IO is a fundamental module to any IO toolkit since it can greatly boost IO performance. In base we have `Handle` type which is powerful but very complex. In stdio we designed a rather simple solution based on the idea from io-streams package: a buffered input device should support push back operation, so that running a resumable `Parser` which only consumes part of the buffer is not a problem. The API of buffered IO is as following:

```haskell
readBuffer :: (HasCallStack, Input i) => BufferedInput i -> IO V.Bytes
readExactly :: (HasCallStack, Input i) => Int -> BufferedInput i -> IO V.Bytes
readToMagic :: (HasCallStack, Input i) => Word8 -> BufferedInput i -> IO V.Bytes
readParser :: (HasCallStack, Input i) => P.Parser a -> BufferedInput i -> IO (ReadResult a)
...
writeBuffer :: (Output o) => BufferedOutput o -> V.Bytes -> IO ()
writeBuilder :: (Output o) => BufferedOutput o -> B.Builder a -> IO ()
flushBuffer :: Output f => BufferedOutput f -> IO ()
...
```

Nothing fancy, every function should work as you expected. Note we don't provide thread-safe guarantees here: to use a `BufferedInput` or `BufferedOutput` in multiple threads, use an `MVar` to protect concurrent access.

More to come
============

For now, stdio is already in its shape. We have put a lot of time on handcrafting over last two years. Lots of experiments, lots of tests. We hope this little project can start to make some of your jobs easier, but of course, it's still in its early stage. There're a lot of functionalities we want to add to it eventually, here is something on the roadmap:
 
+ Implement a binary serialization module similar to binary, cereal package.
+ Implement a JSON module.
+ Support UDP device.
+ Add DNS functionality, i.e `getaddrinfo` binding.
+ Add HTTP protocol support.
+ Add TLS(transfer layer security) support.

Both I and Tao He know that maintain a project with size like this will be out of our capability, so we'd like to ask help from Haskell community here. How can we grow this project further? In which direction our community needs it to be? We'd like to hear your opinions, and of course, your user experience report with stdio!
