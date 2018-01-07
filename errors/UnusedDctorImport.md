# `UnusedDctorImport` Warning

## Example

```purescript
module ShortFailingExample where

import Data.Maybe (Maybe(..))

test :: forall a. Maybe a -> Int
test _ = 42
```

## Cause

A module is imported with an explicit imports list including a type with data construtors (`Maybe(..)` in this example)
but only the type constructor and not its data constructors are used.

## Fix

Import only the type without data constructors. In the above example:

```purescript
import Data.Maybe (Maybe)
```

## Notes

[This error is auto-fixable](../guides/Error-Suggestions.md).
