---
title: "Haskell Meet Transitor: STG machine"
date: 2017/8/25 10:54
tags: haskell
---

如果你去问一个稍有经验的 C 或者是 Java 程序员一段代码是如何在计算机上执行的，八成他/她会把处理器和内存的情况说的一清二楚。就算是很多 JavaScript 程序员也可以和你聊聊 Google V8 的各种优化是如何实现的。而如果你去请教一个 Haskell 程序员他/她的代码是如何运行的，回答很可能是：“我懒得搞得那么清楚”。这种情况和 Haskell 本身的高度抽象当然密不可分，不过更多的原因，在于 Haskell 的执行模型和大多数过程式编程语言存在巨大的差异，作为 Haskell Meet Transitor 系列文章的开篇，我们先来从计算机的角度，聊聊 Haskell 的执行模型。

<!-- more --> 

