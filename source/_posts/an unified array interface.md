---
title: An unified array interface
date: 2017/8/18 13:54
tags: haskell
---

Recently I'm trying to merge [some array code](https://github.com/haskell/primitive/pull/64) from my research project [stdio](https://github.com/winterland1989/stdio) to [primitive](https://github.com/haskell/primitive), one of the core haskell libraries. I hope the reviewing process can be finished soon so that people can start using it. This post is a summary on current GHC array support(up to 8.2.1, things may changed when you read it), and what my patch is about. 

<!-- more --> 

## boxed, unboxed, lifted, unlifted

For many haskellers, using arrays may be the first time one wants to know what's the difference between boxed, unboxed, lifted, unlifted types. Let's spend some time explaining these buzzwords.

In other languages, you often have to distinguish *reference* and *value*. For example, in C pointers are references to other objects. It's a memory location in hardware sense: you can use machine code to follow a reference to the memory it pointing to. While the other non-pointer types value are not memory locations, their 1-0 arrangement stands for a certain value of that type.

In haskell almost every value you see is a pointer in C sense, i.e. a memory location point to a heap object, for example a data type like:

```
data Foo = Foo Int Char

foo = Foo 3 'a'
```

Are represented as:

```
    foo(maybe on stack, maybe on register, maybe inside other boxes)
     |
     V
+----+--------+---+---+    +-------------+------+
| info-table* | * | * +--->+ info-table* | 'a'# |
+-------------+-+-+---+    +-------------+------+
 This is Foo    |          This is C# (Char's constructor)
                V
            +---+---------+----+
            | info-table* | 3# |
            +-------------+----+
             This is I# (Int's constructor)

```

During runtime the value `foo` is just a pointer, and all the operations, e.g. pattern match, is going through dereferencing. Values like this are called *boxed* because it's a pointer to a box, i.e. heap objects with info-table. The info-table contains many useful infomation about the box, such as how many words the boxed occupied, which constructor the box stand for, etc.

The `3#` and `'a'#` above are haskell's non-pointer value, we call values like this *unboxed* values. Unboxed values don't have info-tables, so we really can't have them directly on heap: otherwise the GC would get confused when it scans them, without infomation from info-table, it can't decide how many bytes to copy. These values are only belong to boxes, registers or stacks: we generate machine code to manipulate them directly.

Another difference, unlifted and lifted, exists because in haskell we have non-strict evaluation mechanism, for example a value `1 + 2` may have a representation like:

```
+-------------+----------+---+    +-------------+----+
| info-table* | reserved | * +--->+ info-table* | 2# |
+-------------+----------+---+    +-------------+----+
                                  This is I#

 The info-table points to (+1) code.
```

As you can see, `1 + 2` and `3` are both references, they can be used interchangeably: a function expecting an `Int` argument can accept both pointers. This is done by *entering* the heap objects. i.e. execute the entry code following the info-table. The entry code for constructors are simply returns, since they're already evaluated. For thunks the code will do evacuation and the `reserved` word above is reserved exactly for evaluation result, by writing a forward pointer and change the thunk box into an indirection box.

But the evaluation may fail(diverged recursion, stackoverflow, etc.), then the pointer will point to an undefined value, this kind of things are called *bottom* in haskell, written as `_|_`. The intuition for this name is that all the other evaluated values have certain meaning, but bottom doesn't, it sits lower in the spectrum of determinism, concreteness, usefulness ... whatever suits your mind. Hence comes the concept of `lifted` type, i.e. types which contain `bottom` values, or more formly, inhabited by `_|_`.

As you expected, most of the boxed type can be inhabited by `_|_`, the thunk may explode and terminate your program, just think about `error "!"` or `undefined` in base. What about unboxed types then? Can `Int#` stand for an undefined value? No, it's impossible! All the 1-0 arrangements represent a `Int#`, there's no way we get a bottom from it. 

## boxed arrays

Now let's consider GHC arrays, they're special heap objects provided by RTS. We have boxed arrays `MutableArray#` and `Array#`, they are called boxed arrays because they store references to boxes:

```
+-------------+--------------+---------------------------+---+-...-+---+---+------------+
| info-table* | payload size | payload + card-table size | * | ... | * | * | card table |
+-------------+--------------+---------------------------+-+-+-...-+---+---+------------+
 MutableArray#                                             |
                                                           V
                                                    +------+------+-----+
                                                    | info-table* | ... |                                                        
                                                    +-------------+-----+
                                                      Boxes, maybe a thunk
                                                      boxed array are lazy on its element
```

It looks quite complicated, mainly because we want to optimize the GC for arrays:

+ `MutableArray#` can have different info-table pointers during its life time, but we never enter them. The difference between these info-tables is the type field. e.g. a `MutableArray#` on old generation heap may have `MUT_ARR_PTRS_CLEAN` type which is saying this array have not been mutated since last GC, so if this is a minor GC we can safely skip it.

+ `MutableArray#` have a card table which is just an byte map, recording which part of payload is mutated after last GC, a none-zero byte in card table indicate corresponding payload area(in GHC it's 128 elements) contain mutated pointers, so that GC will trace them. 

`MutableArray#`s are always kept in a generation's mutable list once it's promoted to that generation, so these optimizations are important if you keep a large mutable array on heap for a long time. For arrays smaller than 128, it's unnecessary to use a card-table, so we also have `MutableSmallArray#` for that purpose.

In GHC we usually turn `MutableArray#` into a `Array#` with freeze operations after creation is completely. We changed the info-table(so its type) to `stg_SMALL_MUT_ARR_PTRS_FROZEN0`, then(after a GC) to `stg_SMALL_MUT_ARR_PTRS_FROZEN`, so that minor GC will not scan it anymore. But the card-table's space is still there in case we thaw the array in place again. Generally speaking, under creation-freeze pattern, `MutableSmallArray#` and `SmallArray#` are more recommended since you won't keep the mutable one on heap for too long.

## boxed, unlifted type

`ghc-prim` also provide `MutableArrayArray#` and `ArrayArray#` array type, you can use them to store boxed unlifted values, a boxed unlifted value? Yeah you hear it right, there're certain kind of values, which are pointers pointing to boxes, but them self can never be bottom. It turned out `MutableArray#` and `Array#` are exactly this type of thing. It's easy to tell they're boxed value because they point to boxes we draw above, but why they're unlifted?

`MutableArray#/Array#` are unlifted because **they can't be obtained directly by thunk evaluation**, there're simply no ways to create a thunk which evaluates to `MutableArray#` or `Array#`, you can only create them using primitive operations provided by RTS, and RTS never produce them lazily(actually that may be possible, i.e. allocating array when we enter them, but doing that will complicate things).

What you can do is to wrap the pointer itself inside another box, i.e. `data Array = Array Array#`, and wrap the primitive operations so that an `Array` works like all the other haskell lazy boxed types, this is actually just creating another layer indirection. Now let's say you want an array of arrays, `Array Array` is definitely not optimal: every element of the array points to a `Array` box, and inside the `Array` box we have a `Array#`
points to the real array. The indirection is absolute wasteful if we don't need lazy on the element arrays.

So here come `ArrayArray#`, It's actually just `Array#` s, but we use it to save the `Array#` pointers instead of `Array` pointers, thus save an unnecessary indirection. And we can be sure the element are all evaluated, because `Array#` are unlifted type, which can't be a thunk.

In `primitive` package @dolio push this technique to its limit: we use `ArrayArray#` to store all the boxed unlifted types, such as `MutVar#` or `ArrayArray#` itself. And in my patch i extend this support to `MVar#` and `TVar#`.

## byte arrays

The heap object layout of `MutableByteArray#`, `ByteArray#` are simpler, since they don't contain pointers, we don't have to trace them during GC:

```
+-------------+--------------+-------------+---+-...-+---+---+
| info-table* | payload size | 0x00000000# | # | ... | # | # |
+-------------+--------------+-------------+---+-...-+---+---+
 MutableByteArray#/ByteArray#   
```

In fact we only have one single info-table for both `MutableByteArray#` and `ByteArray#`, thus unlike boxed arrays, freezing and thawing between them in place are just no-ops. Byte array can be used to encode different size non-pointer data, such as `Int` and `Word8`, `ghc-prim` provide seperated functions to work with different data types: `indexIntArray#`, `indexWord8Array#`, etc. In `primitive`, the `ByteArray` type is also accompanied with all these operations. 

My patch add a tagged version of byte array:

```
data MutablePrimArray a = MutablePrimArray MutableByteArray#
data PrimArray a = PrimArray ByteArray#
```

Here `a` can be `Int`, `Word8`, etc. They are just instances of `Prim` class in primitive since a long time ago. The tagged version byte array makes polymorphric unboxed array possible, later it's unified again with another level of abstraction.

Using byte array with different size of `Prim` instances requires user to watch out alignment, GHC RTS allocate byte arrays aligned to machine word, this is sufficient for all the built-in `Prim` instances, but in case of a special alignment requirement, you have to use `newAlignedPinnedByteArray#` primitive operations which do aligned allocation.

You may wonder what's the `pinned` fuzz about. There're two kind of byte arrays, the one which can be moved by GC, and the one which can not. This difference arise when we want to interface foreign code: once we pass a byte array a foreign function call, we don't want it get moved by GC while the foreign function is still doing work on it, so we want to allocate a *pinned* byte array to do the job. 

Allocating pinned byte array are not happening on capability's nursery, it's happening on a global pinned object heap. It's more like a traditional `malloc`. It can be slower to collect, and the allocation may suffer from contentions since we need to aquire a global lock during allocating.

Even though it's slower, we still want to allocate large byte array on that global pinned object heap, we gain performance elsewhere: the GC will not move it. In GHC there's a limit(`LARGE_OBJECT_THRESHOLD`, a little bit smaller than 4K block size), once you are allocating byte array large than that, it will be allocated on pinned heap.

After [#8281](https://ghc.haskell.org/trac/ghc/ticket/8281) is resolved, we now have a guarantee that during unsafe FFI, the GC won't happen. Thus it's safe to pass byte array to an unsafe FFI call(This actually also hold for older GHC, but not for older GHCi). Otherwise you have to ask if a byte array is pinned and create a pinned copy if necessary, new `ghc-prim` provides `isMutableByteArrayPinned#/isByteArrayPinned#`, but on older GHC you can easily get this infomation by asking the byte array's block descriptor flag:

```
int hsprimitive_is_byte_array_pinned(void* p){
    return Bdescr((StgPtr)p)->flags & (BF_PINNED | BF_LARGE);
}
```

## unified array interface

Together with `PrimArray a`, my patch also add a `Arr` class:

```haskell
class Arr (marr :: * -> * -> *) (arr :: * -> * ) a | arr -> marr, marr -> arr where
    -- | Make a new array with given size.
    newArr :: (PrimMonad m, PrimState m ~ s) => Int -> m (marr s a)
    -- | Make a new array and fill it with an initial value.
    newArrWith :: (PrimMonad m, PrimState m ~ s) => Int -> a -> m (marr s a)
    -- | Index mutable array in a primitive monad.
    readArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m a
    -- | Write mutable array in a primitive monad.
    writeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> a -> m ()
    -- | Fill mutable array with a given value.
    setArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> a -> m ()
    -- | Index immutable array, which is a pure operation,
    indexArr :: arr a -> Int -> a
    -- | Index immutable array in a primitive monad, this helps in situations that
    -- you want your indexing result is not a thunk referencing whole array.
    indexArrM :: (Monad m) => arr a -> Int -> m a
    -- | Safely freeze mutable array by make a immutable copy of its slice.
    freezeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (arr a)
    -- | Safely thaw immutable array by make a mutable copy of its slice.
    thawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> Int -> Int -> m (marr s a)
    -- | In place freeze a mutable array, the original mutable array can not be used
    -- anymore.
    unsafeFreezeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> m (arr a)
    -- | In place thaw a immutable array, the original immutable array can not be used
    -- anymore.
    unsafeThawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> m (marr s a)
    -- | Copy a slice of immutable array to mutable array at given offset.
    copyArr ::  (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> arr a -> Int -> Int -> m ()
    -- | Copy a slice of mutable array to mutable array at given offset.
    -- The two mutable arrays shall no be the same one.
    copyMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()
    -- | Copy a slice of mutable array to mutable array at given offset.
    -- The two mutable arrays may be the same one.
    moveArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()
    -- | Create immutable copy.
    cloneArr :: arr a -> Int -> Int -> arr a
    -- | Create mutable copy.
    cloneMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (marr s a)
    -- | Resize mutable array to given size.
    resizeMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m (marr s a)
    -- | Shrink mutable array to given size. This operation only works on primitive arrays.
    -- For boxed array, this is a no-op, e.g. 'sizeOfMutableArr' will not change.
    shrinkMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m ()
    -- | Is two mutable array are reference equal.
    sameMutableArr :: marr s a -> marr s a -> Bool
    -- | Size of immutable array.
    sizeofArr :: arr a -> Int
    -- | Size of mutable array.
    sizeofMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> m Int
    -- | Is two immutable array are referencing the same one.
    sameArr :: arr a -> arr a -> Bool
```

This is a type class trying to unify RTS's array interface, e.g. the `Data.Primitive.XXXArray` modules, it's a multi-parameter class constraining both immutable and mutable array types. For example we have following instances:

```haskell
instance Arr MutableArray Array a where
instance Arr SmallMutableArray SmallArray a where
instance Prim a => Arr MutablePrimArray PrimArray a where
instance PrimUnlifted a => Arr MutableUnliftedArray UnliftedArray a where
```

`Arr` class uses functional dependencies to force a one-to-one immutable/mutable constraint, which is useful since many operations under `Arr` only mention either the immutable array type, or the mutable one.

This class is useful in many ways, for example, array slices can be defined as:

```haskell
-- | Typeclass for box and unboxed vectors, which are created by slicing arrays.
--
class (Arr (MArray v) (IArray v) a) => Vec v a where
    -- | Vector's mutable array type
    type MArray v :: * -> * -> *
    -- | Vector's immutable array type
    type IArray v :: * -> *
    -- | Get underline array and slice range(offset and length).
    toArr :: v a -> (IArray v a, Int, Int)
    -- | Create a vector by slicing an array(with offset and length).
    fromArr :: IArray v a -> Int -> Int -> v a
```

We can provide generic operations without considering which underlined array type is, e.g. a `foldr/build` awared packing function from [stdio](https://github.com/winterland1989/stdio):

```haskell
-- | /O(n)/ Convert a list into a vector with an approximate size.
--
-- If the list's length is large than the size given, we simply double the buffer size
-- and continue building.
--
-- This function is a /good consumer/ in the sense of build/foldr fusion.
--
packN :: forall v a. Vec v a => Int -> [a] -> v a
{-# INLINE packN #-}
packN n0 = \ ws0 -> runST (do let n = max 4 n0
                              mba <- newArr n
                              (IPair i mba') <- foldlM go (IPair 0 mba) ws0
                              shrinkMutableArr mba' i
                              ba <- unsafeFreezeArr mba'
                              return $! fromArr ba 0 i
                          )
  where
    -- It's critical that this function get specialized and unboxed
    -- Keep an eye on its core!
    go :: IPair (MArray v s a) -> a -> ST s (IPair (MArray v s a))
    go (IPair i mba) x = do
        n <- sizeofMutableArr mba
        if i < n
        then do writeArr mba i x
                return (IPair (i+1) mba)
        else do let !n' = n `shiftL` 1
                !mba' <- resizeMutableArr mba n'
                writeArr mba' i x
                return (IPair (i+1) mba')

data IPair a = IPair {-# UNPACK #-}!Int a

```

With `Arr` class, the array programming experience is improved to the "average" level, because in other languages you don't have to use different functions with boxed and unbox array. Now you don't have to do it in haskell either.
