---
title: "[ANN] stdio-0.2.0.0"
date: 2019/5/16 18:18
tags: haskell
---

Today I'm very happy to announce a new beta version of `stdio` is released on hackage, there're some large additions in this version:

* Add UDP module.
* Add JSON module.
* Add `ToText` class to `Std.Data.TextBuilder` module.

The largest addition is a new JSON module, features [full RFC 8259 compliance](https://github.com/haskell-stdio/JSONTestSuite) and [2~3x faster performance](https://github.com/haskell-stdio/stdio/tree/master/bench/json). Try to bring big improvements over a very mature library like aeson is not an easy task, parts of the improvements come from faster `Parser` and `Builder` infrastructures in `stdio`. But we also made a few other improvements:

+ To improve building lookup table during JSON parsing(and many other formats as well), An ordered map module based on sorted vectors and binary-search is also added, there're actually four modules providing similar functions to `containers`:

    + `Std.Data.Vector.FlatMap`
    + `Std.Data.Vector.FlatIntMap`
    + `Std.Data.Vector.FlatSet`
    + `Std.Data.Vector.FlatIntSet`

These data structures have different time and memory characteristics from tree-based ones, and suits some situations better, e.g. intermediate lookup table.

+ To improve JSON string parsing, we made an UTF-8 validation state machine with JSON unescaping logic built-in written in C, combine with the ability to scan a whole chunk of input(provided by our new `Parser`, namely `scanChunks`), we now have faster JSON string parsing performance than some native code(`JSON.parse` from nodejs).

+ Numeric parsing is also improved, e.g. we changed `Integer` decoding loop to use machine words if range permits, then covert the result to `Integer` rather than use `Integer` all the time.

There're also lots of improvements and simplifications contribute to faster performance. Overall this is good proof that our new `Parser` and `Builder` infrastructures work well.

Besides the JSON module, another important addition is the `ToText` class in `Std.Data.TextBuilder`, which is also derivable via generics. The format is chosen to follow `Show` class as much as possible, this is a very handy class when you need to quickly turn something into a builder or text.

A UDP module is added quite easily since the IO manager part is in place. Due to the message based nature, the API is quite different from TCP, design suggestions are welcomed!

Happy hacking!
Cheers
