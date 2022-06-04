# `UnusedName` Warning

## Example

```purescript
module UnusedNameExample where

plus :: Int -> Int -> Int
plus x y = y -- Name x was introduced but not used.

ignore :: forall a. a -> Unit
ignore value = unit -- Name value was introduced but not used.
```

## Cause

This warning occurs when a name is introduced but is not used anywhere.

PureScript warns in this case because it could indicate a bug due to a value not being referenced where it should be.

## Fix

If the unused name was unintentional, make use of it to fix the warning:

```purescript
plus :: Int -> Int -> Int
plus x y = x + y
```

If the unused name is intentional, mark the name as unused by prefixing it with a `_`:

```purescript
ignore :: forall a. a -> Unit
ignore _value = unit
```

## Notes
