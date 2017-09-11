---
title: "Haskell Bottom Up：The STG Machine"
date: 2017/8/25 10:54
tags: haskell
---

很多 Haskeller 无法一眼看穿自己的代码调度计算机的过程，而这种情况很少发生在 C 或者是 Java 程序员身上，常常一个普通的 JavaScript 程序员也可以和你聊聊 Google V8 的各种优化是如何实现的。这种情况和 Haskell 本身的高度抽象当然密不可分，不过更多的原因，在于 Haskell 的执行模型和大多数过程式编程语言的区别，作为 Haskell Meet Transitor 系列文章的开篇，我们先来聊聊 Haskell 的主力实现 GHC， 是如何编译 Haskell 程序的。

<!-- more --> 

## STG 自动机

STG 是一门面向代码生成的函数式语言，也是一个十分有趣的自动机，其设计目标是在现有的硬件架构（冯·诺依曼）上高效地求值 STG 表达式，它包含：

+ STG 寄存器，一组虚拟寄存器（会尽量分配到真实寄存器上）：
    + `Hp` 堆顶寄存器，记录当前堆顶的地址。
    + `HpLim` 堆底寄存器，记录当前最大栈地址。
    + `Sp` 栈顶寄存器，记录当前栈顶的地址。
    + `SpLim` 栈底寄存器，记录当前最小栈地址。
    + `R1..R10, F1..F4, D1..D4, L1..L2` 临时结果寄存器，用于传递参数、结果。

+ STG 栈，一个地址向下增加的栈。

+ STG 堆，一个地址向上增加的堆。

这些 STG 寄存器的硬件架构约定在[MachRegs.h](https://github.com/ghc/ghc/blob/master/includes/stg/MachRegs.h)里。STG 的运行就是通过不停地对 STG 表达式求值实现的，所以 STG 程序的单元是 *表达式*（expression）而非 *语句*（statement）。其实在传统的编译体系，例如 C 语言里，也存在 *函数* 的概念，这个概念其实包含了代码生成过程中的几个要点：

+ 函数体里的代码里如何找到函数的参数？

+ 如何控制硬件进入函数执行？

+ 函数体里的临时变量存放在哪里，怎样保证函数结束之后得到正确的清理？

+ 函数体的返回值放在哪里？

运行时的调用约定（calling convention）就是对上述问题的解答。对于 STG 来说，除了一些可以直接通过机器指令实现的函数之外，其他情况下和 C 差不多，例如参数一般都在栈上，如果处理器的寄存器比较富裕的话也会通过寄存器来传递参数等等。另外 STG 函数还需要一些扩展功能：支持自由变量和柯里化。即 STG 除了要定义上述问题的答案之外，还需要额外定义如下问题的答案：

+ 函数体里的代码如何找到函数的自由变量，也就是通过词法作用域引入而非参数传递的绑定。

+ 函数期待的参数数目（arity）是多少，如果调用的时候提供的参数数量小于或者大于期待的数目，应该如何处理？

STG 的设计还需要考虑到运行时的其他组件，例如垃圾回收器（GC）和多线程调度器（scheduler）。这就需要通过一些约定来保证彼此的正常工作。

## STG 内存布局

我们先来了解一下 STG 中的代码和数据是如何在现有计算机的内存架构上存放的。假如是让我来设计一门函数式编程语言的话，一个简单的想法是通过如下的内存布局来表示函数：

```
+--------------+---+--...--+
| Code pointer | * |  ...  |
+------+-------+-+-+--...--+
       |         |    ...
       V         V    VVV
     函数体      自由变量
```

然后通过下面的布局来表示数据：

```
+-------------+---+--...--+
| tag pointer | * |  ...  |
+-------------+-+-+--...--+
  数据的 tag    |    ...
                V    VVV
             数据的 payload
```

其实这个布局也是很多简单的函数式编程语言所采用的，即在生成函数体代码的时候把所有的自由变量的引用添加到函数指针的后面，这样在函数体里的代码就可以通过函数指针和偏移量来定位这些自由变量了。数据的表示就更容易理解了：类似 C 语言的 `struct` 和 `enum`，我们也是通过一个起始地址和后续的偏移量来表示一组相关数据的集合（在代数数据类型里叫做积），如果数据有不同的分支，我们就通过一个额外的 tag
word 来记录是哪一个分支（在代数数据类型里叫做和），所有的数据都是通过引用传递的。

STG 采用的是一种更加统一的内存布局来表示函数和数据，在 STG 里我们把这些内存里的物体叫做*闭包*（这可能和你之前了解到的闭包的概念不大相同），当然 STG 也支持非引用的数据类型，例如整数、浮点数等等，不过这不影响我们的讨论。目前 GHC 用到的闭包类型结构定义可以在 [Closures.h](https://github.com/ghc/ghc/blob/master/includes/rts/storage/Closures.h) 里找到，尽管这些不同的闭包结构体有着各种各样的字段，其中有一些字段是一定存在的。换句话说，STG 的闭包具有统一的内存布局：

![heap-object](https://ghc.haskell.org/trac/ghc/raw-attachment/wiki/Commentary/Rts/Storage/HeapObjects/heap-object.png)

上图就是所有 STG 闭包都满足的一个通用布局，即一个信息头（info-header）指针，以及后续的 payload，info-header 指针指向的是一个被称之为信息表（info-tablel）的静态数据结构，由编译器生成，存放着关于闭包的很多描述信息，展开来看整个信息表的基本结构如下：

![info-table](https://ghc.haskell.org/trac/ghc/raw-attachment/wiki/Commentary/Rts/Storage/HeapObjects/basic-itbl.png)

最重要的字段是闭包类型（closure type），GHC 里用到的闭包类型都在 [ClosureTypes.h](https://ghc.haskell.org/trac/ghc/browser/includes/rts/storage/ClosureTypes.h) 里定义好了，这个字段描述了闭包是函数还是数据，或者是一个待计算的任务盒。对于不用的闭包类型，其他的字段的解释也有所不同，例如布局（layout）这个字段，就有两种情况：

+ 对于某些闭包来说，payload里的数据是按照先引用类型后原始类型排列的（记得 STG 支持非引用类型？）。这种情况下 layout 字段对应两个 half-word，分别表示了 payload 里的引用数量和原始类型数据数量。

+ 对于某些闭包来说引用类型和原始类型是混合排列的，例如栈桢（stack-frame），这个时候 layout 字段对应的是一个 bitmap，用位的 0/1 来表示对应的 payload 是否是指针。

紧跟在信息表后面的是闭包对应的机器码（entry code）。对于函数来说，闭包的 payload 就包含了所有的自由变量。而对于数据来说，payload 就是数据的字段，数据的不同的分支通过闭包的信息头指向不同的信息表实现。其实对于数据来说，也会有一段机器码，这个在文章后面在细说。当我们需要求取一个闭包的值的时候，我们会跳转到闭包对应的信息表后面的机器码里，这个过程叫做进入闭包（enter the closure）。

因为函数体的代码都是事先生成的，所以中间使用到的引用都已经静态生成了，所以布局这个字段更多的是给 GC 使用的，当 GC 扫描到一个闭包的时候，可以通过读取布局追踪到该闭包引用到的其他闭包。

## STG 的执行流程

STG 的运行目标是求解 STG 表达式的值，具体的做法如下：

+ 下一步要执行的代码压入栈顶（其实也是一个闭包），这个闭包被称为 continuation。

+ `jmp`进入闭包，执行闭包的机器码就是求值的过程，这其中可能会包括压入新的 continuation，进入新的闭包。

+ `R1` 里指向求值过程中待进入的闭包，以及求值完毕之后的结果。

+ 如果存在通过栈传递的参数，continuation 的代码需要在返回之前修改 `Sp` 指针出栈。

当整个表达式求值完毕程序就结束了。下面我们分开讨论 STG 支持的几种重要的求值方式。

## 函数调用

STG 里的函数调用和 C 类似是支持多参数的，调用过程同样是先把参数压入栈中，紧接着`jmp`进入函数体，如果有超过一个参数要入栈的话，STG 在进行函数调用之前会进行栈剩余空间的检查，如果剩余空间不足以压入全部参数，会触发 GC 并把 R1 指向到静态生成的一个任务盒（也就是这次函数调用），GC 结束之后进入 R1。

STG 使用 eval/apply 来支持函数的柯里化，简单说来就是：

+ 任何函数的参数个数是确定的，并被记录在了信息表里。

+ 如果调用方传递的参数正好满足了函数所需的参数个数，则

+ 如果调用方传递的参数个数大于函数所需的参数个数，则入栈一个应用剩余参数的闭包，再把函数所需的参数压入栈，进入函数闭包。




## 构建和解构（construction & destruction）

基于上述的内存布局，STG 直接提供了强大的代数数据类型的构建和解构，例如 Haskell 的 `data` 和 `case` 语法就直接会被翻译成 STG 的构建和解构：

```haskell
data Foo = Foo Int Int | Bar Char

...
    let foo = Foo 2 3
...
    case foo of Foo x y -> ...
                Bar z   -> ...
...
```

构建的过程非常简单：

+ 如果是静态构建，则直接生成闭包的，并使用该闭包的地址作为闭包的值。
+ 如果是动态构建，先检查堆空间是否足够（Hp + closure_size > HpLim），足够则直接分配内存创建，并进入闭包对应的机器码。否则会进入 GC 调用，之后再回到构建过程。

不难看出，如果希望构建数据对应的闭包





## 任务盒 Thunk









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


