# `CannotDerive` Error

## Example

```purescript
data Bool = True | False

derive instance heytingAlgebraBool :: HeytingAlgebra Bool
```

## Cause

This error shows up when you're attempting to derive an instance for which compiler
support does not exist.

## Fix

- You will need to write an instance yourself.
