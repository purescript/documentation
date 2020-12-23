# `DuplicateRoleDeclaration` Error

## Example

```purescript
module ShortFailingExample where

data D a = D a
type role D nominal
type role D nominal
```

## Cause

Multiple role declarations are provided for the same type.

## Fix

- Remove the extraneous role declarations:

```diff
 data D a = D a
 type role D nominal
-type role D nominal
```
