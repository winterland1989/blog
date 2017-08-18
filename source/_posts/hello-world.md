---
title: An unified array interface
date: 2017/8/18 13:54
tags: wtf
---

Recently I'm trying to merge [some array code](https://github.com/haskell/primitive/pull/64) from my research project [stdio](https://github.com/winterland1989/stdio) to [primitive](https://github.com/haskell/primitive), one of the core haskell libraries. I hope the reviewing process can be finished soon so that people can start using it. This post is a summary on this patch.

## The array problem

In haskell we usually not use array as often as in other languages, the reason is simple: it's awkward to use comparing other languages. The default `[]` syntax is left to list, which is one the most important algebraic data types in haskell. It can be pattern matched, be polymorphric on the element type, and be manipulated with simple recursion. 

But the underline memory representation of list is linked-list, or rather single branch tree, which limited its usage under high performance scenario: we either have to fuse it so that there're no list at all, or consuming it in a lazy manner so that the garbage collector can chase to collect outdated elements. In other use case, you'd better ask for 


while array is implemented in RTS, and exposed 


