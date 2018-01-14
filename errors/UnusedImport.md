# `UnusedImport` Warning

## Example

```purescript
module ShortFailingExample where

import Some.Module

-- Nothing from Some.Module is ever used
```

## Cause

There is a statement `import Some.Module` which is not required, as nothing from `Some.Module` is used (with the same qualifier if any).

## Fix

Remove the import statement.

## Notes

Often occurs during development when the code using the import has not yet been written, in which case can be ignored. 

[This error is auto-fixable](../guides/Error-Suggestions.md).
