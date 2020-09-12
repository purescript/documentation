# `CannotFindCoercedCtor` Error

## Example

```purescript
module N (N) where

newtype N a = N a
```

```purescript
module ShortFailingExample where

import Safe.Coerce (coerce)
import N

example :: forall a. a -> N a
example = coerce
```

## Cause

Newtype constructors have to be in scope to allow coercions to and from their representation.

## Fix

- Export the newtype constructor:

```diff
-module N (N) where
+module N (N(..)) where
```
