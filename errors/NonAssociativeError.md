# `NonAssociativeError`

## Example

```purescript
module ShortFailingExample where

import Prelude ((==), (/=))

example = true == false /= true
```

Result:
```
Cannot parse an expression that uses multiple non-associative operators of the same precedence:

  Data.Eq.(/=)
  Data.Eq.(==)

Use parentheses to resolve this ambiguity.
```

## Cause

This error arises when a group of non-associative operators with the same precedence are used in such a way that the compiler cannot be sure how to bracket them unambiguously. In the example above, `==` and `/=` from Prelude are both declared as `infix 4` (non-associative, precedence 4), so it's not clear whether this expression should be bracketed as `(true == false) /= true` or `true == (false /= true)`.

This error will not arise in the case of repeated applications of operators which have been declared as `infixl` or `infixr`, as in this case, the fixity declaration removes the ambiguity by specifying which way expressions should be bracketed. See [Syntax - Binary Operators](../language/Syntax.md#binary-operators) in the language reference for more information.

## Fix

The most appropriate fix is usually to add brackets to remove the ambiguity:

```purescript
(true == false) /= true
```

An alternative fix is to change one or more of the operators' fixities or precedences so that the compiler can determine how expressions involving these operators should be bracketed without extra guidance.
