# `CannotFindCoercedNewtypeCtor` Error

## Example

```purescript
module N (N(..)) where

newtype N a = N a
```

```purescript
module ShortFailingExample where

import Safe.Coerce (coerce)
import N (N)

example :: forall a. a -> N a
example = coerce
```

## Cause

Newtype constructors have to be in scope to allow coercions to and from their representation.

## Fix

- Import the newtype constructor:

```diff
-import N (N)
+import N (N(..))
```
