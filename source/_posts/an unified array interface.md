---
title: An unified array interface
date: 2017/8/18 13:54
tags: haskell
---

Recently I'm trying to merge [some array code](https://github.com/haskell/primitive/pull/64) from my research project [stdio](https://github.com/winterland1989/stdio) to [primitive](https://github.com/haskell/primitive), one of the core haskell libraries. I hope the reviewing process can be finished soon so that people can start using it. This post is a summary on this patch, and the current situation on haskell array programming.

<!-- more --> 

## boxed, unboxed, lifted, unlifted

For many haskellers, using arrays may be the first time one wants to know what's difference between boxed, unboxed, lifted, unlifted type. I'd like to spend some time explaining these buzzwords.

In other languages, you often have to distinguish *reference* and *value*. For example, in C pointers are references to other objects, you can create pointers for almost every type: `int`, `size_t`, a custom `struct` with custom layout, etc. even pointer itself can be referenced by creating a reference to it. It's a memory location in hardware sense: you can use machine code to follow a reference to the memory it pointing to. While the other non-pointer types value are not memory locations, their 1-0 arrangement stands for a certain value of that type.

In haskell almost every value you see is a pointer in C sense, i.e. a memory location point to a heap object, for example a data type like:

```
data Foo = Foo Int Char

foo = Foo 3 'a'
```

Are represented as:

```
    foo(maybe on stack, maybe on register, maybe on other's closure)
     |
     V
+----+-------+---+---+    +------------+------+
| infotable* | * | * +--->+ infotable* | 'a'# |
+------------+-+-+---+    +------------+------+
 This is Foo   |           This is C# (Char's constructor)
               V
            +--+---------+----+
            | infotable* | 3# |
            +------------+----+
             This is I# (Int's constructor)

```

During runtime the value `foo` is just a pointer, and all the operations, e.g. pattern match, is going through dereferencing. Values like this are called *boxed* because it points to boxes, i.e. heap objects with info-table. The info-table contains many useful infomation about the box, such as how many words the boxed occupied, which constructor the box stand for, etc.

The `3#` and `'a'#` above are haskell's way to represent non-pointer value, we call values like this *unboxed* values. Unboxed values don't have info-tables, so we really can't have them directly on heap: otherwise the garbage collect would get confused when it scans them, without infomation from info-table, it can't decide how many bytes to copy. These values are only belong to boxes, registers or stacks: we generate machine code to manipulate them
directly.

Here comes a another difference, in haskell we have non-strict evaluation mechanism, for example a value `foo = 1 + 2` may have a representation like:

```
+------------+----------+---+    +------------+----+
| infotable* | reserved | * +--->+ infotable* | 2# |
+------------+----------+---+    +------------+----+
                                  This is I#

 The info-table points to (+1) code.
```

As you can see, `foo = 1 + 2` and `3` are both references, they can be used interchangeably: a function expecting an `Int` argument can accept both pointers. This is done by *entering* the heap objects. i.e. execute the entry code following the info-table. The entry code for constructors are simply returns, since they're already evaluated. For thunks the code will do evacuation and the `reserved` word above is reserved exactly for evaluation result. 

But the evaluation may fail(diverged recursion, stackoverflow, etc.), the pointer may point to an undefined value, this kind of things are called *bottom* in haskell, written as `_|_`. The intuition for this name is that all the other evaluated values have certain meaning, while this one doesn't, it just sits lower in the spectrum of determinism, concreteness, usefulness ... whatever suits your mind. Hence comes the concept of `lifted` type, i.e. types which contain `bottom` values, or more formly, inhabited by `_|_`.

As we can see, all boxed type can be inhabited by `_|_` since the thunk may explode and terminate your program, just think about `error "!"` or `undefined` in base, they are all examples of bottom. What about unboxed types then? Can `Int#` stand for an undefined value? No, it's impossible! All the 1-0 arrangements represent a `Int#`, there's no way we get a bottom from it.

## boxed arrays

Now let's consider GHC arrays, they're special heap objects provided by RTS. We have boxed arrays `MutableArray#` and `Array#`, they are called boxed arrays because they store references to boxes:

```
+------------+--------------+---------------------------+---+-...-+---+---+------------+
| infotable* | payload size | payload + card-table size | * | ... | * | * | card table |
+------------+--------------+---------------------------+-+-+-...-+---+---+------------+
  `MutableArray#`                                         |
                                                          V
                                                    +-----+------+-----+
                                                    | infotable* | ... |                                                        
                                                    +------------+-----+
                                                      Boxes, maybe a thunk
```

It looks quite complicated, mainly because we want to optimize the garbage collecting for arrays:

+ `MutableArray#` can have different info-table pointers during its lift time, but we never entere them, see reasons below. The difference between these info-tables is the type field. e.g. a `MutableArray#` on old generation heap may have `MUT_ARR_PTRS_CLEAN` type which is saying this array have been mutated since last GC, so if this is a minor GC we can safely skip it.

+ `MutableArray#` have a card table which is just an byte map, recording which part of payload is mutated after last GC, a none-zero byte in card table indicate corresponding payload area(in GHC it's 128 elements) contain mutated pointers, so that GC will copy those 128 pointer all together. 

`MutableArray#`s are always kept in a generation's mutable list once it's promoted to that generation, so these optimizations are important if you keep a large mutable array on heap for a long time. For arrays smaller than 128, it's unnecessary to use a card-table, so we also have `MutableSmallArray#` for that purpose.

In GHC we usually turn `MutableArray#` into a `Array#` with freeze operations after creation is completely. We changed the info-table(so its type) to `stg_SMALL_MUT_ARR_PTRS_FROZEN0`, then(after a GC) to `stg_SMALL_MUT_ARR_PTRS_FROZEN`, so that minor GC will not scan it anymore. But the card-table's space is still there in case we thaw the array in place again. So under creation-freeze pattern, `MutableSmallArray#` and `SmallArray#` are recommended.

## boxed, unlifted type

`ghc-prim` also provide `MutableArrayArray#` and `ArrayArray#` array type, you can use them to store boxed unlifted values, wait a minute, a boxed unlifted value? Yeah you hear it right, there're certain kind of values, they're pointers pointing to boxes, but it self can never be bottom. It turned out `MutableArray#` and `Array#` are exactly this type of thing. It's easy to tell they're boxed value because they point to boxes we draw above, but why they're unlifted?

`MutableArray#/Array#` are unlifted because **they can't be obtained directly by thunk evaluation**, there're simply no ways to create a thunk which produce a `MutableArray#` or `Array#`, you can only create `Array#` using primitive operations provided by RTS, and RTS never produce a lazy `Array#`(actually that may be possible, i.e. allocating array when we enter them, but doing that will complicate things since allocated address may change).

What you can do is to wrap the pointer itself inside another box, i.e. `data Array = Array Array#`, and wrap the primitive operations so that an `Array` works like all the other haskell boxed types, this is actually just creating another layer indirection, to solve the lazy allocating problem we put above. Now let's say you want an array of arrays, `Array Array` is definitely not optimal: every element of the array points to a `Array` box, and inside the `Array` box we have a `Array#`
points to the real array. The indirection is absolute wasteful if we don't need lazy on the element arrays.

So here come `ArrayArray#`, It's actually just `Array#` s, but we use it to save the `Array#` pointers instead of `Array` pointer, thus save an unnecessary indirection. And we can be sure the element are all evaluated, because `Array#` are unlifted type, which can't be a thunk.

In `primitive` package @dolio push this technique to its limit: we now can use `ArrayArray#` to store all the boxed unlifted types, such as `MutVar#` or `ArrayArray#` itself. And in my patch i extend support to `MVar#` and `TVar#`. 

## byte arrays


The heap object layout of `MutableByteArray#`, `ByteArray#` are simpler, since they don't contain pointers, we don't have to trace them during GC:

```
+------------+--------------+-------------+---+-...-+---+---+
| infotable* | payload size | 0x00000000# | # | ... | # | # |
+------------+--------------+-------------+---+-...-+---+---+
  `MutableByteArray#/ByteArray#`   
```

In fact we only have one single info-table for both ``MutableByteArray#` and `ByteArray#`, thus freezing and thawing between them in place are just no-ops. A problem with `ByteArray#` is that it can be used to encode different size non-pointer data, such as `Int` and `Word8`, `ghc-prim` provide seperated functions to work with different data types: `indexIntArray#`, `indexWord8Array#`, etc. In `primitive`, the `ByteArray` type is also accompanied with all these operations. But my patch add
support for a tagged version: `PrimArray a`, where `a` can be `Int`, `Word8`, etc. (It's actually the just instances of `Prim` class in primitive since long time ago).

Another problem with `ByteArray#` is alignment, GHC RTS allocate byte arrays aligned to machine word, this is sufficient for all the built-in `Prim` class, but in case of a special alignment requirement, you have to use `newAlignedPinnedByteArray#` primitive operations which do aligned allocation.

You may wonder what's the `pinned` fuzz about `ByteArray#`, there're two kind of `ByteArray#`s, the one which can be moved by GC, and the one which can not. This difference arise when we want to interfacing FFI code: once we pass a `ByteArray#` a foreign function call, we don't want it get moved by GC while the foreign function is still doing work on it, so we want to allocate a *pinned* `ByteArray#` to do the job. But allocating pinned `ByteArray#` are not happening on capability's
nursery, it's happening on a global pinned object heap. It's more like a traditional `malloc`. It will be slower to collect, and the allocation may suffer from contentions since we need to aquire a global lock during allocating.

Even though it's slower, we still want to allocate large `ByteArray#` on that global pinned object heap, we gain performance because garbage collected will not move it. In GHC there's a limit(`LARGE_OBJECT_THRESHOLD`, a little bit smaller than 4K block size), once you are allocating `ByteArray#` large than that, it will be allocated on pinned heap.


## The problem with `[]`

In haskell we usually not use array as often as in other languages, the reason is simple: it's awkward to use comparing other languages. The default `[]` syntax is left to list, which is one the most important algebraic data types in haskell. It can be pattern matched, be polymorphric on the element type, and be manipulated with simple recursion. 

But the memory representation of list is linked-list, or rather single branch tree, which has a great impact on its runtime characteristics:

+ The memory overhead of list is extremely high, we have to save extra pointers which point to next cell and the current cell's value. The situation is worse since heap object have an extra info-table pointers, for example a `[5]` look like this in memory:

```
+------------+---------------+-------------------+    +------------+
| info-table | value pointer | next cell pointer +--->+ info-table | 
+------------+-------+-------+-------------------+    +------------+
 This is (:)                   |                        This is []
                     V
                +----+-------+----+
                | info-table | 5# |
                +------------+----+
                 This is 5
```

+ We can't do O(1) index, just as all the other single linked-list.

These characteristics in turn limit list's usage under high performance scenario: we either have to fuse it so that there're no list at all, or consuming it in a lazy manner so that the garbage collector can chase to collect outdated elements. In another words: you should never fully materialized a list, it's better to be consumed in one go.

This is basically saying that you should never share a large list, doing that will simply trash your memory. for example, following code blow up your memory easily:

```
countDown = [1000, 999...0]

forM countDown $ \ i ->
    doSomethingFoo

forM countDown $ \ i ->
    doSomethingBar
```

The `countDown` list get materialized during first loop, and the second loop's existence stop it being get garbage collected timely. If you're luck enough, GHC may decide to inline `countDown` into each loop, then you can hope the fusion happen, but we really don't want to rely on inliner's judgement on this case.

