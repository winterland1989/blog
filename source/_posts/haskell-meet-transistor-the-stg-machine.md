---
title: "Haskell Bottom Up：The STG Machine"
date: 2017/8/25 10:54
tags: haskell
---

    To tag or not to tag: that is the question:
    Whether 'tis faster in the cache to suffer
    The delays of tagless nodes,
    Or break the pipe no more
    And make a branch that hits the cache.
    To load, to jump;
    To jump: perchance to stall; Ay, there's the run.
                    
                                                -- Kevin Hammond

<!-- more -->

## STG 是什么？

STG 是一门迷你的函数式编程语言，以及配套的自动机设计。它的主要设计目的是把 Haskell 的程序变成可以在现有的硬件架构（冯·诺依曼）上运行的机器码，在整个 GHC 编译流程中的位置大约如下图：

```
Haskell ====> Core ====> STG ====> Cmm ====> Code
```

作为 GHC 使用的一个中间语言，他并没有提供可以直接用于书写的语法，但是提供了简单的打印功能。你可以通过向 `ghc` 传递 `-ddump-stg` 参数来观察一个 haskell 程序对应的 STG 程序表示。

作为一门函数式编程语言，STG 支持下几种简单的语法结构：

+ 数据构造 constructor
+ 数据解构 case
+ 数据字面量 literal
+ 原始操作/FFI primitives
+ 匿名函数 lambda
+ 函数应用 application
+ Let绑定 let

同时作为一个自动机体系，STG 提供了上述语法结构的执行模型，即生成 Cmm 代码的规则（Cmm 是 GHC 使用的一层汇编抽象层以屏蔽不同机器架构的区别）。

虽说 STG 是一门编译器的中间语言，但是它的抽象程度其实相对来说已经相当的高了。例如它支持自由变量，即其他编程语言里闭包的概念；他还支持函数的自动柯里化，通过参数数量分析自动生成对应的调用代码。这些都是很多传统的过程式编程语言所不支持的。

值得强调的是，STG 的程序是一个巨大嵌套的表达式，而不像过程式编程语言的程序那样，按照语句顺序编译可以直接得到一个巨大的指令列表。事实上 STG 的一个目标就是把这个巨大的表达式转换成一个按步骤求值的指令列表。

## STG 自动机

STG 自动机是一套把 STG 程序转换成底层顺序执行代码的规则，概念上来说，它包含：

+ STG 寄存器，其中重要的有：
    + `Hp` 堆顶寄存器，记录当前堆顶的地址。
    + `HpLim` 堆底寄存器，记录当前最大栈地址。
    + `Sp` 栈顶寄存器，记录当前栈顶的地址。
    + `SpLim` 栈底寄存器，记录当前最小栈地址。
    + `R1..R10, F1..F4, D1..D4, L1..L2` 临时结果寄存器，用于传递参数、结果。

+ STG 栈，地址向下增加。

+ STG 堆，地址向上增加。

STG 里的虚拟寄存器会在被尽量分配到真实寄存器上以提高性能，如果

这些 STG 寄存器的硬件架构约定在[MachRegs.h](https://github.com/ghc/ghc/blob/master/includes/stg/MachRegs.h)里。


STG 的运行就是通过不停地对 STG 表达式求值实现的，所以 STG 程序的单元是 *表达式*（expression）而非 *语句*（statement）。其实在传统的编译体系，例如 C 语言里，也存在 *函数* 的概念，这个概念其实包含了代码生成过程中的几个要点：

+ 函数体里的代码里如何找到函数的参数？

+ 如何控制硬件进入函数执行？

+ 函数体里的临时变量存放在哪里，怎样保证函数结束之后得到正确的清理？

+ 函数体的返回值放在哪里？

对上述问题的答案也被称作行时的调用约定（calling convention）。对于 STG 来说，除了一些可以直接通过机器指令实现的函数之外，其他情况下和 C 差不多，例如参数一般都在栈上，如果处理器的寄存器比较富裕的话也会通过寄存器来传递参数等等。另外 STG 函数还需要一些扩展功能：支持自由变量和柯里化。即 STG 除了要定义上述问题的答案之外，还需要额外定义如下问题的答案：

+ 函数体里的代码如何找到函数的自由变量，也就是通过词法作用域引入而非参数传递的绑定。

+ 函数期待的参数数目（arity）是多少，如果调用的时候提供的参数数量小于或者大于期待的数目，应该如何处理？

STG 的设计还需要考虑到运行时的其他组件，例如垃圾回收器（GC）和多线程调度器（scheduler）。这就需要通过一些约定来保证彼此的正常工作。


## 数据构造，数据解构

大部分函数式编程语言的控制流程都建立在数据的构造和解构之上。例如下面的这个 Haskell 声明：

```haskell
data Bool = Ture | False
```

定义了一个*数据类型`Bool`*，这个类型有两个可能的*构造函数(constructor) `True`, `False`*，当我们想要知道一个`Bool`类型的值是哪一个构造函数的时候，我们使用`case`语法进行模式匹配:

```haskell
case (f x) of True  -> ....
              False -> ...
```

这就完成了其他语言内置的`if x then a else b`逻辑控制结构，Haskell 本身也提供了 `if x then a else b` 语法，但是它不过是下面的语句的语法糖而已：

```
case x of True  -> a
          False -> b
```

构造的过程除了可以把选择编码进去，也可以把数据集合起来，例如下面的 Haskell 声明：

```haskell
data Point = Point Double Double
```

声明了一个数据类型`Point`，以及它对应的构造函数`Point`（这两个`Point`并不冲突，前一个在类型的命名空间里，后一个不在）。当我们需要解构一个`Point`包含的成员时，我们同样使用模式匹配：

```
case (f a) of Point x y -> ...这里我们就可以使用 x 和 y 了
```

从某种角度来讲，构造函数构造数据的过程和模式匹配是两个相反的过程，对于没有内置控制结构的 Haskell 来说，这就是我们控制程序走向的基本方式。数学家们把上面的两种构造数据的过程分别叫做 *和/sum* 和 *积/product*



在函数式编程里，我们非常关注构造（construction）和解构（deconstruction）的过程，例如下面的数据类型：

```haskell
`data JSON 
    = Object (HashMap Text JSON)
    | Array  [JSON]
    | String Text
    | Number Double
    | Bool   Bool
    | Null
```

不难看出这是一个表示 JSON 数据的类型，基于这个类型构造出来的数据，非常易于解构，例如序列化的时候：

```
case json of Object obj -> ... 递归解构 obj ... 
             Array  arr -> ... 递归解构 arr ...
             String str -> ... 序列化 str ...
             ...
```







## STG 自动机

STG 支撑 Haskell 在现有的硬件架构（冯·诺依曼）上运行的自动机体系，也是理解很多 Haskell 语义的关键。对于第一次接触过函数式编程语言的朋友们来说，这可能是一个奇怪的说法：Haskell 的程序是由表达式构成的一个大表达式。但是结合多年 `hello world!` 的经验，如果我告诉你，Haskell 的程序实际上是 `main` 表达式，你可能会感觉更加良好一些：

```haskell

main :: IO ()
main = print "hello world!"
```

看上去和其他语言也没什么不同嘛，不过其他的语言里常常会有*语句`statement`*的概念：例如，在 C 语言里使用`;`隔开的就是语句，语句是 C 程序的基本构成单元，C 的程序运行顺序即语句的书写顺序。

这一切在 Haskell 里都变了，Haskell 是一个基于 lambda 演算的语言：简单地说，就是一门建立在函数定义和函数调用之上的语言。Haskell 本身的语法尽管庞大，但在编译阶段都会通通转换成 STG，而 STG 是一门微型的函数式语言，它可以被高效的分析和运行。你可能还在疑惑，一个只有函数的语言能干些什么呢？我们不妨来了解下 STG，这门微型的语言包括几个核心的组成部分：

+ 字面量表达式，例如整数，浮点数，字符，数组，例如
+ 原始操作表达式，例如加减乘除，或者是操作系统提供的 I/O 函数
+ 函数定义，STG 里的函数都是匿名函数/lambda
+ Let 绑定，也就是给表达式起个名字
+ 数据构造，
+ Case 分析，

  = StgApp         occ [GenStgArg occ] 
  | StgLit         Literal
  | StgConApp      DataCon [GenStgArg occ] [Type]        
  | StgOpApp       StgOp [GenStgArg occ] Type            
  | StgLam         [bndr] StgExpr  
  | StgCase        (GenStgExpr bndr occ) bndr AltType [GenStgAlt bndr occ]
  | StgLet         (GenStgBinding bndr occ) (GenStgExpr bndr occ)    
  | StgLetNoEscape (GenStgBinding bndr occ) (GenStgExpr bndr occ)  
  | StgTick (Tickish bndr) (GenStgExpr bndr occ)      


它包含：

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

对上述问题的答案也被称作行时的调用约定（calling convention）。对于 STG 来说，除了一些可以直接通过机器指令实现的函数之外，其他情况下和 C 差不多，例如参数一般都在栈上，如果处理器的寄存器比较富裕的话也会通过寄存器来传递参数等等。另外 STG 函数还需要一些扩展功能：支持自由变量和柯里化。即 STG 除了要定义上述问题的答案之外，还需要额外定义如下问题的答案：

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


