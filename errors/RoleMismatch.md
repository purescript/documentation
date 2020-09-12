# `RoleMismatch` Error

## Example

```purescript
module ShortFailingExample where

data D a = D a
type role D phantom
```

```purescript
module ShortFailingExample where

data D f a = D (f a)
type role D representational representational
```

## Cause

A parameter was assigned a role more permissive than required.

## Fix

- Remove or strengthen the role declaration:

```diff
 data D a = D a
-type role D phantom
+type role D representational
```

```diff
 data D f a = D (f a)
-type role D representational representational
+type role D representational nominal
```
