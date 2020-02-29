# `UnsupportedTypeInKind` Error

## Example

```purescript
foreign import data Bad :: SomeConstraint => Type
```

## Cause

Not all types are valid in a kind signature. Particularly constraint arrows
(`=>`) are disallowed since they only make sense for term-level constraint
dependencies.

## Fix

- Only use types that are valid in kinds.
