---
title: Haskell Meet Transitor: the compile pipeline
date: 2017/8/25 10:54
tags: haskell
---

时常发现很多 Haskeller 无法一眼看穿自己的代码调度计算机的过程，而这种很少发生在 C 或者是 Java 程序员身上，常常一个普通的 JavaScript 程序员也可以和你聊聊 Google V8 的各种优化是如何实现的。这种情况和 Haskell 本身的高度抽象当然密不可分，不过更多的原因，在于 Haskell 的执行模型和大多数过程式编程语言的区别，作为 Haskell Meet Transitor 系列文章的开篇，我们先来聊聊 Haskell 的主力实现 GHC， 是如何编译 Haskell 程序的。

<!-- more --> 




## Core 的语法糖

我们熟知并热爱的 Haskell 语法



##


STG machine 是 STG 语言在冯·诺依曼体系计算机下的执行模型（execution model），



https://www.microsoft.com/en-us/research/wp-content/uploads/1992/04/spineless-tagless-gmachine.pdf
