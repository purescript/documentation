# `OverlappingArgNames` Error

## Example

```purescript
> f x x = x
Error found:
in module $PSCI
at line 1, column 5 - line 1, column 13

  Overlapping names in function/binder in declaration f
```

## Cause

This error occurs when you attempt to bind the same variable to different function arguments.

## Fix

- Rename one of the duplicate function arguments.

## Notes
