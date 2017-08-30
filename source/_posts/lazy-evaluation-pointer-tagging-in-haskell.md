---
title: Inside GHC: 惰性求值和指针标记优化
date: 2017/8/25 10:54
tags: haskell
---

一个常常被问起的 Haskell 问题是：[Haskell中的惰性求值如何实现？](https://www.zhihu.com/question/23849854)，真正解答这个问题除了需要《计算机原理》这门课不挂科之外，还需要按照 Haskell 的社区传统，去阅读若干篇 paper（见文章底部引用）。这篇文章尝试对 Haskell 的主流实现，GHC 的惰性求值做一个总结，并附带介绍 GHC 实现的一种优化:指针标记（pointer tagging），来帮助读者对惰性求值的 cost model
有一个更好的判断。

<!-- more --> 

## 什么是惰性求值？

在上一篇文章 [An unified array interface](/2017/08/18/an-unified-array-interface/) 里，我简单的提到过了 Haskell 中的引用和值的区别，大意就是 Haskell 里面大部分的数据都是以引用（指针）的方式存在的，还是具下文的例子：

```
data Foo = Foo Int Char

foo = Foo 3 'a'
```

在内存中大约这样：

```
    foo(一个引用，栈上，或寄存器你，或其他的盒子里)
     |
     V
+----+--------+---+---+    +-------------+------+
| info-table* | * | * +--->+ info-table* | 'a'# |
+-------------+-+-+---+    +-------------+------+
  这是 Foo      |          这是 C# (Char 的构造函数)
                V
            +---+---------+----+
            | info-table* | 3# |
            +-------------+----+
             This is I# (Int 的构造函数)

```

在运行时，代码中`foo`的值就是一个指针，任何的操作诸如模式匹配、作为参数传递都是通过这个指针来进行的，`foo`指向的盒子其实就是 Haskell 中大部分 Value 的存在形式。当然也有 `3#` 和 `'a'#` 这样的非引用类型的值，不过在 Haskell 里我们常常把他们包装起来使用：

```
data Char = C# Char#
data Int = I# Char#
```

这里一个关键的指针是每个盒子最开始的那个 info-table 指针，顾名思义，info-table 就是信息表，这是一个大约长成这样的结构体（定义在 [InfoTables.h](https://ghc.haskell.org/trac/ghc/browser/includes/rts/storage/InfoTables.h)）：

```
     假定这是 Foo 类型的值，编译器会为 Foo 的每一个构造生成一个 info-table
    +-------------+----...             +------------+
    | info-table* | payload            | layout     |
    +------+------+----...             +------------+
           |                           |  type      |
           |                           +------------+
           |                           | srt bitmap |
           +-------------------------->+------------+
                                       |   code     |
                                       :     :      :
                                       :     :      :
```

信息表会被编译器写进最终的二进制可执行文件，执行时载入到内存里，每当你的程序里使用了`Foo`的构造函数时，就会造出一个盒子，而盒子的 info-table 指针就会指向这个结构体。这里的`layout`和`srt bitmap`是 GC 非常关心的两个值，在后续讲到 GHC 的 GC 的时候我再详细解释，`type` 这个字段指的是对应盒子的类型，常见的类型有：

+ `CONSTR`，一个通过构造函数动态创建的盒子，例如某个函数体里返回的`Foo x y`。

+ `CONSTR_STATIC`，一个编译器静态创建的盒子，例如顶层的`Foo 3 'a'`绑定。

+ `FUN`, `FUN_STATIC`，动态/静态创建的函数，例如 `\x y -> x + y`。

+ `THUNK`, `THUNK_STATIC`















# 参考

+ [GHC Commentary](https://ghc.haskell.org/trac/ghc/wiki/Commentary)
