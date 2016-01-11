You likely encountered this because you were porting some lazy Haskell code to PureScript.

Purescript is strictly evaluated so such constructs are a compile error.

If you are trying to implement a recursive parser in PureScript you might find 
[the fix function](http://pursuit.purescript.org/packages/purescript-string-parsers/0.6.3/docs/Text.Parsing.StringParser.Combinators#v:fix) and this post on
[recursive parsing in PureScript](https://github.com/Thimoteus/SandScript/wiki/2.-Parsing-recursively) helpful.