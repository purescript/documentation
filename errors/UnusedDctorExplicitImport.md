# `UnusedDctorExplicitImport` Warning

## Example

```purescript
module ShortFailingExample where

import Data.Maybe (Maybe(Just, Nothing))

test = Nothing
```

## Cause

A module is imported with an explicit imports list including a type with an explicit list of data construtors
(`Maybe(Just,Nothing)` in this example) but some of the mentioned data constructors are unused.

## Fix

Import only the used data constructors:

```purescript
import Data.Maybe (Maybe(Nothing))
```

Alternatively import all data constructors non-explicitly:

```purescript
import Data.Maybe (Maybe(..))
```

## Notes

[This error is auto-fixable](../guides/Error-Suggestions.md).
