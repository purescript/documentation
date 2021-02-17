# `PossiblyInfiniteCoercibleInstance` Error

## Example

```purescript
module ShortFailingExample where

import Safe.Coerce (class Coercible, coerce)

newtype N a = N (a -> N a)

infinite :: forall a b. Coercible a b => N a -> N b
infinite = coerce
```

## Cause

Solving diverges for recursive newtypes: here, the previous example yields the wanted `Coercible (N a) (N b)` which we unwrap on both sides to yield `Coercible (a -> N a) (b -> N b)` and then decompose back to `Coercible a b` and `Coercible (N a) (N b)`.

## Fix

We cannot unwrap newtypes constructors unless they're in scope, so moving the newtype declaration to a separate module and importing only its type prevents the loop.

```diff
+module N where
+
+newtype N a = N (a -> N a)
```

```diff
+import N (N)
-newtype N a = N (a -> N a)
```

## Notes

Note the intervening `->` constructor in the declaration of the newtype. This is actually the issue here because we only unwrap _finite_ chains of newtypes.

For instance given the following declaration:

```purescript
newtype N a = N (N a)
type role N representational
```

When solving a wanted `Coercible (N a) (N b)` we don't unwrap but instead decompose the wanted to `Coercible a b`, which can then eventually be discharged by the context.
