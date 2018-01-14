# `ImportHidingModule` Error

## Example

```purescript
module ShortFailingExample where

import B hiding (module A)
```

## Cause

An `import` statement has a `hiding` list containing a `module` reference. Hiding imports cannot be used to hide modules.

## Fix

Remove the `hiding` item.

## Notes
