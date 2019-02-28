---
title: Aeson's mysterious lazy parsing
date: 2019/03/05 01:55
tags: haskell
---

Today I'm hacking on a new JSON parser for my stdio package, of course i reused quite a lot of code from aeson. Kudos, Bryan O'Sullivan! While this time i really hacked into the core, I learned something i never really though about. 

Aeson provide two flavor of parsing entrances: `decode` and `decode'` (and similarly `eitherDecode` and `eitherDecode'`). The document on `decode` says that `This function parses immediately, but defers conversion.`. I have never actually even though about it carefully because this looks quite obvious to a Haskell veteran like me. `defers conversion`? It must be the costs of convert numbers, strings, arrays, etc. are too high, so that during parsing we suspend the conversion and return a result containing thunks. So if our JSON instances only use part of the keys, we can save some time.

OK, that's my interpretation of those docs for a long time, and i always prefer `decode` to `decode'` since thunking those conversions makes sense to me. But when i started to hack a JSON value parser from ground, suddenly I realized this is far more complex than my first look, a question is begging me to answer:

    How could be this `conversion defering` possible, when parsing should tell us if the JSON is valid? 

There must be something wrong here. After parsing we should have something like `Either String a`, if we defer the conversions, how could we know it's `Right`? If the defered conversion somehow met problems, and we already give a `Right` result, a bottom `undefined` will be produced? No this's just unacceptable, we must do all the conversions before we can be sure the result is `Right`. But what's the point of this lazy vs strict arrangement then?

Reading source quickly leads us to the difference before [`json` and `json'`](https://github.com/bos/aeson/blob/master/Data/Aeson/Parser/Internal.hs#L78) parser: i.e. it's the conversions between JSON bytes and `Value`, aeson's intermediate JSON representation, makes this differences. In `json` case,  thunks are produced during parsing. But where're those thunks? What makes things more mysterious is that `Value` use strict field all the way:

```haskell
type Object = HashMap Text Value
type Array = Vector Value

-- | aeson's intermediate JSON representation
data Value = Object !Object
           | Array !Array
           | String !Text
           | Number !Scientific
           | Bool !Bool
           | Null
             deriving (Eq, Read, Show, Typeable, Data, Generic)
```

Look at those strict fields! It means that if we force a `Value` to WHNF, e.g. to know if it's a `Number` or a `String`, we have to force the `Scientific` or `Text` payload. The only possible places for thunks are **the payload of the payload** of `Object`s and `Array`s, which are payloads of a `HashMap` or a `Vector` respectively. So during building these `HashMap`s and `Vector`s, we write thunks instead of WHNF data structures. But why the laziness doesn't bring bottom problem? If we have parsed the whole JSON bytes without pack results into `HashMap` or `Vector`, so where're they? Reading the source of internal lazy parsers reveal the answer:

```haskell
arrayValues :: Parser Value -> Parser (Vector Value)
arrayValues val = do
  skipSpace
  w <- A.peekWord8'
  if w == CLOSE_SQUARE
    then A.anyWord8 >> return Vector.empty
    else loop [] 1
  where
    loop acc !len = do
      v <- val <* skipSpace
      ch <- A.satisfy $ \w -> w == COMMA || w == CLOSE_SQUARE
      if ch == COMMA
        then skipSpace >> loop (v:acc) (len+1)
        else return (Vector.reverse (Vector.fromListN len (v:acc)))
```

`Parser` is classic CPS parser from `attoparsec` package, In CPS parser's world, a parser will parse its own part and call next parser if itself runs successfully. To get a successful parsing result, we have to run though all the parsers in a line, convert everything into a proper result and pass on. Look at the `loop` above, it will parse all the elements of a JSON array into a list before call next continuation. Now the important part comes:

```haskell
    return (Vector.reverse (Vector.fromListN len (v:acc)))
```

Since aeson uses accumulator style to build the list, a `reverse` is neccessary, but this monadic `return` is not strict! so we build a thunk to defer the vector building process, but the list of elements is already there, in our memory, built as the parsing loop goes. So yes all parsing work is done, and we don't have to worry about bottoms coming from defered conversions: the only defered conversion here is just a conversion between list of elements to vector of elements, which should never fail.

Similarly in `HashMap` case, we have already parsed all the key-values into a list of key-value pairs. The loop body of the `Object` parser is like this:

```haskell
  loop acc = do
    k <- str <* skipSpace <* char ':'
    v <- val <* skipSpace
    ch <- A.satisfy $ \w -> w == COMMA || w == CLOSE_CURLY
    let acc' = (k, v) : acc
    if ch == COMMA
      then skipSpace >> loop acc'
      else return (H.fromList acc')
```

Now things are clear, we never miss a thing during parsing, and we only defered two conversions when using aeson's lazy paring. Now the question becomes how does this laziness helped things? Will if we modify [the benchmarks](https://github.com/bos/aeson/blob/master/benchmarks/AesonParse.hs#L30) in aeson's repo, change the `json` parser to a strict `json'`, suddenly performance dropped by ~40%.

```
# change to use a strict parser
    json-data/twitter1.json :: 60000 times
0.8 KB: 68836 msg\/sec (56.1 MB\/sec)
    json-data/twitter10.json :: 13000 times
6.4 KB: 11712 msg\/sec (73.7 MB\/sec)
    json-data/twitter20.json :: 7500 times
11.8 KB: 5568 msg\/sec (64.1 MB\/sec)
    json-data/twitter50.json :: 2500 times
31.2 KB: 2045 msg\/sec (62.3 MB\/sec)
    json-data/twitter100.json :: 1000 times
61.5 KB: 927 msg\/sec (55.7 MB\/sec)
    json-data/jp10.json :: 4000 times
14.6 KB: 6087 msg\/sec (87.0 MB\/sec)
    json-data/jp50.json :: 1200 times
44.1 KB: 1751 msg\/sec (75.4 MB\/sec)
    json-data/jp100.json :: 700 times
82.9 KB: 820 msg\/sec (66.3 MB\/sec)

# now is the lazy parser
    json-data/twitter1.json :: 60000 times
0.8 KB: 97350 msg\/sec (79.4 MB\/sec)
    json-data/twitter10.json :: 13000 times
6.4 KB: 17675 msg\/sec (111.1 MB\/sec)
    json-data/twitter20.json :: 7500 times
11.8 KB: 8692 msg\/sec (100.0 MB\/sec)
    json-data/twitter50.json :: 2500 times
31.2 KB: 3171 msg\/sec (96.6 MB\/sec)
    json-data/twitter100.json :: 1000 times
61.5 KB: 1392 msg\/sec (83.6 MB\/sec)
    json-data/jp10.json :: 4000 times
14.6 KB: 8435 msg\/sec (120.5 MB\/sec)
    json-data/jp50.json :: 1200 times
44.1 KB: 2430 msg\/sec (104.6 MB\/sec)
    json-data/jp100.json :: 700 times
82.9 KB: 1119 msg\/sec (90.6 MB\/sec)
```

This big performance gap lies on `Vector` and `HashMap` building, some further benchmarks indicate it's the `HashMap` building which is costing: calculating hashes and build a HAMT trees is not a easy job. If everything stops at this stage, and we never access fields of the `HashMap`, then we can say laziness have improved our performance. But this is just a false positive like many others, as long as we want to access a single field from the `HashMap`, the thunk will be entered and building cost will be paid, even if you don't want every field of the `HashMap`, of course the `Value` nested inside the field still may contain thunks, but we're actually not saving time in common case, where the final step of JSON parsing is to covert a JSON `Value` to a Haskell record. 

From the memory perspective, lazy parsing is more problematic: we are hold lists instead of packed `Vector`s or `HashMap`, which means many indirection pointers and boxes can not be GCed, all the nodes of the JSON document is parsed anyway, and they are holded within Haskell lists, which are holded by conversion thunks. That's just awful!

My conclusion: avoid use lazy parsing in aeson if possible, it's not worked like what I expected, and brings no benefits in most of the case.
