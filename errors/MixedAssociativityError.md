# `MixedAssociativityError`

## Example

```purescript
module ShortFailingExample where

import Prelude ((<$>), (==))

feq f x y = f <$> x == f <$> y
```

Result:
```
Cannot parse an expression that uses operators of the same precedence but mixed associativity:

  Data.Functor.(<$>) is infixl
  Data.Eq.(==) is infix

Use parentheses to resolve this ambiguity.
```

## Cause

This error arises when an expression involves operators of the same precedence but different associativities. In this example, `<$>` is `infixl 4`, that is, left-associative with precedence 4, whereas `==` is `infix 4`, that is, non-associative with precedence 4. The compiler is unable to determine how to bracket this expression because of the precendences and associativities involved; either the associativities must be the same or the precedences must be different. See [Syntax - Binary Operators](../language/Syntax.md#binary-operators) in the language reference for more information.

## Fix

The most appropriate fix is usually to add brackets to remove the ambiguity:

```
feq f x y = (f <$> x) == (f <$> y)
```

An alternative fix is to change the precedence of one of the operators, in order to allow the expression to be parsed without adding parentheses.
