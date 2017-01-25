# `OverlappingPattern` Error

## Example

```purescript
not :: Boolean -> Boolean
not true  = false
not _     = true
not false = true
```

## Cause

This error occurs when a pattern matching definition has **redundant** patterns, i.e., overlapping clauses.

In the example, notice that the third case is redundant (that is, unreachable), as it is covered by the second case (which covers all remaining cases).

## Fix

- Remove any redundant cases from the declaration.

## Notes
