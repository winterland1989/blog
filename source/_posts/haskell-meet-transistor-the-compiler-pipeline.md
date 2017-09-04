---
title: "Haskell Meet Transitor: The compiler pipeline"
date: 2017/8/25 10:54
tags: haskell
---

时常发现很多 Haskeller 无法一眼看穿自己的代码调度计算机的过程，而这种很少发生在 C 或者是 Java 程序员身上，常常一个普通的 JavaScript 程序员也可以和你聊聊 Google V8 的各种优化是如何实现的。这种情况和 Haskell 本身的高度抽象当然密不可分，不过更多的原因，在于 Haskell 的执行模型和大多数过程式编程语言的区别，作为 Haskell Meet Transitor 系列文章的开篇，我们先来聊聊 Haskell 的主力实现 GHC， 是如何编译 Haskell 程序的。

<!-- more --> 

##  编译过程

了解编译过程对于掌握 Haskell 程序的运行时表现至关重要，尤其是像 GHC 这样会对

对 Haskell 的[编译过程](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/HscMain) 较为复杂，
主要包括以下几个阶段：

+ 解析（parse）、重命名（rename）、类型检查（type checker）

和其他大部分语言一样，解析先后经过了 [lexer](https://github.com/ghc/ghc/blob/master/compiler/parser/Lexer.x) 和 [parser](https://github.com/ghc/ghc/blob/master/compiler/parser/Parser.y) 的处理，得到了使用 `RdrName` （其实就是字符串）标注的抽象语法树（AST）： `HsModule RdrName`, `HsExpr RdrName`, `HsBinds RdrName`... 语法错误会在这一步的时候会抛出。

重命名这一步会分析词法作用域，给 AST 中的不同的 `RdrName` 分配不同的唯一数，用来在后续的处理中区分相同和不同的名称，AST 的标记类型也变成了 [`Name`](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/NameType#TheNameSortofaName)。这一步还会展开 QuasiQuotes，因为 QuasiQuotes 展开的时候可能会影响到词法作用域。和词法作用域相关的错误会在这一步抛出。

类型检查这一步会遍历整个 AST，收集类型约束（Constraint），然后运行复杂的约束求解器（Constraint Solver）。这一步还会展开除了 QuasiQuotes 之外其他的 Template，类型错误会在这一步抛出。

由于 Haskell 的语法异常庞大，而 type checker 中的各项 typing rules 又要覆盖整个 AST ，这导致了 GHC 的 type checker 异常复杂，充满了许多 adhoc 的 rules 来满足限制性语法的需求，

类型检查期间一个非常重要的变换是展开类型类（type-class）的词典（dictionary），举个简单的例子：

```
class Show a where
    show :: a -> String
    showList :: [a] -> String
    ...

instance Show Foo where
    show = showFoo
    ...

bar :: Show a => a -> ...
bar x = ... show x ...

bar (foo :: Foo)
```

会被处理成：

```
-- | 这个数据类型被称作 Show 的词典类型
data ShowDict a = ShowDict 
    { show :: a -> String
    , showList :: [a] -> String
    ... 
    }

-- | 这个类型对应的就是 Foo 的 Show 实例词典
showFooInstance :: ShowDict
showFooInstance = ShowDict 
    { show = showFoo
    ...
    }

-- | 每一个 class 约束都会被替换成一个额外的词典参数
-- 每一处 class 方法都被替换成词典中对应的 field
bar :: ShowDict 
bar showdict x = ... (show showdict) x ...

-- | 在调用方把正确的词典传递给多态方法
bar showFooInstance foo
```

+ 脱糖（Desugar）



编译过程在 [GHC Commentary](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/HscMain) 上已经总结的非常清楚了，这里提几个比较重要的点：

![GHC compile pipeline](https://ghc.haskell.org/trac/ghc/raw-attachment/wiki/Commentary/Compiler/HscPipe/HscPipe2.png)


## Core 的语法糖

我们熟知并热爱的 Haskell 语法



##


STG machine 是 STG 语言在冯·诺依曼体系计算机下的执行模型（execution model），



https://www.microsoft.com/en-us/research/wp-content/uploads/1992/04/spineless-tagless-gmachine.pdf
+ `HsSyn` AST，这是
