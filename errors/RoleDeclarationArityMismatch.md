# `RoleDeclarationArityMismatch` Error

## Example

```purescript
module ShortFailingExample where

data D a = D a
type role D nominal nominal
```

## Cause

The arity of a role declaration does not match the arity of its corresponding type.

## Fix

- Remove the extraneous roles:

```diff
 data D a = D a
-type role D nominal nominal
+type role D nominal
```
