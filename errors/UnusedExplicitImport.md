# `UnusedExplicitImport` Warning

## Example

```purescript
module ShortFailingExample where

import Data.Maybe (maybe, Maybe(..))

test = Just -- maybe is not used
```

## Cause

A module is imported with an explicit imports list but not all of the identifiers are used.

## Fix

Remove the unnecessary identifiers as suggested.

## Notes

[This error is auto-fixable](../guides/Error-Suggestions.md).