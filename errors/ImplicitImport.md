# `ImplicitImport` Warning

## Example

```purescript
module ShortFailingExample where


import Prelude
import Data.Maybe

z = Just (-1)
```

## Cause

A module is imported implicitly (without an explicit import list), and there is another implicit import present (which may or may not be `hiding`).

## Fix

Make all but at most one import explicit. In the above example we could make either `Data.Maybe` or `Prelude` explicit, e.g.

```
import Prelude (negate)
```

## Notes

[This error is auto-fixable](../guides/Error-Suggestions.md).