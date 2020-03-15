# `OrphanKindDeclaration` Error

## Example

```purescript
data Proxy :: forall k. k -> Type
```

## Cause

A top-level kind signature is provided but not immediately followed by it's declaration.

## Fix

- Check that you've provided the appropriate `data`, `newtype`, `type`, or `class` declaration.
- Check that the keywords match.
