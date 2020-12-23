# `OrphanRoleDeclaration` Error

## Example

```purescript
module ShortFailingExample where

type role D nominal
```

## Cause

A role declaration is provided but does not immediately follow it's declaration.

## Fix

- Check that you've provided the appropriate `data` or `newtype` declaration.
