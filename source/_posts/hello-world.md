---
title: An unified array interface
date: 2017/8/18 13:54
tags: wtf
---

Recently I'm trying to merge [some array code](https://github.com/haskell/primitive/pull/64) from my research project [stdio](https://github.com/winterland1989/stdio) to [primitive](https://github.com/haskell/primitive), one of the core haskell libraries. I hope the reviewing process can be finished soon so that people can start using it. This post is a summary on this patch.

## Why we need array?

In haskell we usually not use array as often as in other languages, the reason is simple: it's awkward to use comparing other languages. The default `[]` syntax is left to list, which is one the most important algebraic data types in haskell. It can be pattern matched, be polymorphric on the element type, and be manipulated with simple recursion. 

But the underline memory representation of list is linked-list, or rather single branch tree, which has a great impact on the runtime characteristics:

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

These characteristics in turn limit list's usage under high performance scenario: we either have to fuse it so that there're no list at all, or consuming it in a lazy manner so that the garbage collector can chase to collect outdated elements. In another words: you should not to fully materialized a list, it's better to be consume in one go.

This is basically saying that you should never share a list if it's large, doing that will simply trash your memory.

while array is implemented in RTS, and exposed 


