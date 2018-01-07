# `DuplicateImportRef` Warning

## Example

```purescript
module ShortFailingExample where

import Prelude (unit, unit)

test = unit
```

## Cause

An explicit import list contains multiple references to the same thing, i.e. has duplicates.

## Fix

Remove the extraneous items.

## Notes
