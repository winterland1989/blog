---
title: A builder with multiple strategy 
date: 2017/8/18 13:54
tags: haskell
---

In [stdio](https://github.com/haskell-stdio/stdio) a new `Builder` is added to match the new `ByteArray` based bytes type. Here is the formular:

```
newtype Builder a = Builder
    { runBuilder :: forall s. AllocateStrategy s -> (a -> BuildStep s) -> BuildStep s}

type BuildStep s = Buffer s -> ST s [Bytes]

-- Here is our Bytes type, aiming at replacing `ByteString`.
type Bytes = PrimVector Word8
data PrimVector a = PrimVector (PrimArray a) Int Int
```

I choose monadic interface over monoid interface, because it's simple to add a `Monoid` instance for `Builder ()` once we provide the `Monad` instance for `Builder`. Let's break down the formular:

+ A `BuildStep` gives everything you need to build a `Bytes` (or list of `Bytes` if you want), it's supposed to write something into the `Buffer`, and 

+ `Builder a` should wrap a `BuildStep s -> BuildStep s`

+ `AllocateStrategy`



```
-- | 'AllocateStrategy' will decide how each 'BuildStep' proceed when previous buffer is not enough.
--
data AllocateStrategy s
    = DoubleBuffer       -- Double the buffer and continue building
    | InsertChunk {-# UNPACK #-} !Int   -- Insert a new chunk and continue building
    | OneShotAction (V.Bytes -> ST s ())  -- Freeze current chunk and perform action with it.
                                        -- Use the 'V.Bytes' argument outside the action is dangerous
                                        -- since we will reuse the buffer after action finished.
```

data Buffer s = Buffer {-# UNPACK #-} !(A.MutablePrimArray s Word8)  -- well, the buffer content
                       {-# UNPACK #-} !Int  -- writing offset


