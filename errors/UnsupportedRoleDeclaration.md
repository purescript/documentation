# `UnsupportedRoleDeclaration` Error

## Example

```purescript
module ShortFailingExample where

class C a
type role C representational
```

```purescript
module ShortFailingExample where

data D a = D a

type S a = D a
type role S nominal
```

## Cause

A role declaration is provided for a type synonym or a type class.

## Fix

- Remove the role declaration or provide a role declaration for the underlying type of the synonym instead:

```diff
 data D a = D a
+type role D nominal
 
 type S a = D a
-type role S nominal
```
