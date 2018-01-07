# `DuplicateImport` Warning

## Example

```purescript
module ShortFailingExample where

import Data.Maybe
import Data.Maybe

test = Nothing
```

## Cause

The same module has been imported multiple times (with the same qualification if any).

## Fix

Remove one of the extra import statements.

## Notes

[This error is auto-fixable](../guides/Error-Suggestions.md).
