# `CannotDefinePrimModules` Error

## Example

```purescript
module Prim where
```

```purescript
module Prim.ShortFailingExample where
```

## Cause

The Prim namespace is reserved by the compiler.

## Fix

- Rename the module to move it outside the Prim namespace:

```diff
-module Prim.ShortFailingExample where
+module ShortFailingExample where
```
