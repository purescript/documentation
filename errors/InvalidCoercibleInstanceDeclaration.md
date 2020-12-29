# `InvalidCoercibleInstanceDeclaration` Error

## Example

```purescript
module ShortFailingExample where

import Prim.Coerce (class Coercible)

instance coercible :: Coercible a b
```

## Cause

`Coercible` instances are solved by the compiler and cannot be declared.

## Fix

- Remove the instance declaration.
