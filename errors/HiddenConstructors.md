# `HiddenConstructors` Warning

## Example

```purescript
module ShortFailingExample (N) where

import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)

newtype N a = MkN a

derive instance newtypeN :: Newtype (N a) _
derive instance genericN :: Generic (N a) _
```

## Cause

Instances of `Newtype` and `Generic` allow to match values of their type with `unwrap` and `from`, and to construct them with `wrap` or `to` hence making the constructors public.

## Fix

- Export the constructors:

```diff
-module ShortFailingExample (N) where
+module ShortFailingExample (N(..)) where
```

- or remove the instances:

```diff
 module ShortFailingExample (N) where

-import Data.Newtype (class Newtype)
-import Data.Generic.Rep (class Generic)

 newtype N a = MkN a

-derive instance newtypeN :: Newtype (N a) _
-derive instance genericN :: Generic (N a) _
```
