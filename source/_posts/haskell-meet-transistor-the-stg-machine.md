---
title: "Haskell Bottom Up：ata"
date: 2017/8/25 10:54
tags: haskell
---

时常发现很多 Haskeller 无法一眼看穿自己的代码调度计算机的过程，而这种很少发生在 C 或者是 Java 程序员身上，常常一个普通的 JavaScript 程序员也可以和你聊聊 Google V8 的各种优化是如何实现的。这种情况和 Haskell 本身的高度抽象当然密不可分，不过更多的原因，在于 Haskell 的执行模型和大多数过程式编程语言的区别，作为 Haskell Meet Transitor 系列文章的开篇，我们先来聊聊 Haskell 的主力实现 GHC， 是如何编译 Haskell 程序的。

<!-- more --> 

## STG 自动机

STG 是一个有趣的自动机，设计目标是在现有的冯·诺依曼上高效地求值 STG 表达式，它包含：

+ STG 寄存器，一组虚拟寄存器（会尽量分配到真实寄存器上）：
    + Hp 堆顶寄存器，记录当前堆顶的地址。
    + HpLim 堆底寄存器，记录当前最大栈地址。
    + Sp 栈顶寄存器，记录当前栈顶的地址。
    + SpLim 栈底寄存器，记录当前最小栈地址。
    + R1..R10, F1..F4, D1..D4, L1..L2 临时结果寄存器，用于传递参数、结果。

+ STG 栈，一个地址向下增加的栈。

+ STG 堆，一个地址向上增加的堆。

代码和数据是以*闭包*为单元组织的，STG 里的闭包是一个非常宽泛的概念，目前用到的闭包类型定义可以在 []() 里找到，STG 的闭包具有统一的内存布局：

![heap-object](https://ghc.haskell.org/trac/ghc/raw-attachment/wiki/Commentary/Rts/Storage/HeapObjects/heap-object.png)






## 一门简单的函数式语言

STG 也是一门非常简单的函数式编程语言，在 GHC 的编译流程中， STG 程序使用如下的数据结构表示：

```haskell
data GenStgExpr bndr occ
  = StgApp         occ [GenStgArg occ] 
  | StgLit         Literal
  | StgConApp      DataCon [GenStgArg occ] [Type]        
  | StgOpApp       StgOp [GenStgArg occ] Type            
  | StgLam         [bndr] StgExpr  
  | StgCase        (GenStgExpr bndr occ) bndr AltType [GenStgAlt bndr occ]
  | StgLet         (GenStgBinding bndr occ) (GenStgExpr bndr occ)    
  | StgLetNoEscape (GenStgBinding bndr occ) (GenStgExpr bndr occ)  
  | StgTick (Tickish bndr) (GenStgExpr bndr occ)      
```

一股浓浓的 lambda 演算味儿扑面而来，除了直观上就很容易理解的 `StgLam`, `StgApp`, `StgLit` 这几种表达式节点之外，STG 的设计还有以下一些值得注意的点：

+ STG 的程序是由 `GenStgExpr` 构成的一张图，对最外层的 `GenStgExpr` 求值过程就是程序的运行过程。

+ STG 中的 let 形式 `StgLet` 中的 binding 部分对应了 allocation，因为 `GenStgBinding` 的定义如下：

```
data GenStgBinding bndr occ
  = StgNonRec bndr (GenStgRhs bndr occ)
  | StgRec    [(bndr, GenStgRhs bndr occ)]

data GenStgRhs bndr occ
  = StgRhsClosure
        CostCentreStack        
        StgBinderInfo          
        [occ]                  
        !UpdateFlag            
        [bndr]                 
        (GenStgExpr bndr occ)  
```

根据是否存在 recursive binding，`GenStgBinding` 被分成了两类。每个 binding 的右侧 `GenStgRhs` 都对应了一个实打实的 closure，也就是我们常说的任务盒（thunk），任务盒对应的是图上另一个需要被求值的 `GenStgExpr` 节点。

+ 根据 closure 是否被共享，



+ STG 中的函数应用 `StgApp` 的参数是 `GenStgArg` :

```haskell
data GenStgArg occ
  = StgVarArg  occ
  | StgLitArg  Literal
```

也就是说 STG 中的函数应用的参数只能是变量或者是字面量，其他类型的表达式要通通绑定到某个变量名上才能作为参数。

+ `StgLam` 仅在 Core 到 Stg 的转换过程中存在，在转换完毕了之后就被绑定到模个名称上成为 `StgLet`：

```haskell


```

+ AST 的节点上会点缀上 binder， 一般来说就是 `Id` 类型的 identifier ，在从 Core 到 Stg 转换的过程中，所有的

+ 和 occurrence 信息便于指导后续的代码生成

+ `StgApp`， 这是一个 STG 中的函数应用，根据 occurrence 是否为 `OneShot` 会有不同处理。

+ `StgLit`，这是一个字面量表达式。

+ `StgConApp`，*饱和应用*了的构造函数。

+ `StgOpApp`，*饱和应用*了的原始类型函数（primitive operations）或者 foreign call。

+ `StgLam`，STG 里的一个匿名函数。

+ `StgCase`， STG 里的一个 case 表达式。

+ `StgLet`， let 表达式。

+ `StgLetNoEscape`， 



## Lambda App

## 构造和解构

大部分函数式编程语言都会提供构造和解构数据的手段，STG 当然也不例外，


```haskell
data Circle = Circle Int Int
```

让我们先看个 C 语言里，组织数据的例子：

```c
struct Books {
   char  title[50];
   char  author[50];
   char  subject[100];
   int   book_id;
} book;  

enum color { red, yellow, green, blue };
```

大部分人都知道 C 语言里的结构体(struct)和枚举(enum)类型不过是内存里的一堆 0 和 1 罢了，但是这些语法的出现，却大大方便了我们处理数据，因为我们可以用定义出来的结构体或者枚举，去表示结构化的数据，让



最先要介绍的语法，是数据的声明`data`。这是整个 Haskell 的核心


GHC 中的**闭包(closure)**概念和你之前脑海的设定可能会有些区别，简单说来，GHC 中的闭包指的是内存中的一类数据结构，例如下面的表达式：

```
data Either a b = Left a | Right b

```

## 解构


