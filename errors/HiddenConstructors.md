# `HiddenConstructors` Warning

## Example

```purescript
module ShortFailingExample (N) where

import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)

newtype N a = N a

derive instance newtypeN :: Newtype (N a) _
derive instance genericN :: Generic (N a) _
```

## Cause

Instances of `Newtype` and `Generic` allow to match values of their type with `unwrap` and `from`, and to construct them with `wrap` or `to` hence making the constructors public.

## Fix

- Export the constructors or remove the instances.
