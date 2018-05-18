# `PartiallyAppliedSynonym` Error

## Example

```purescript
module ShortFailingExample where

newtype UserNT f = UserNT { firstName :: f String }
type TypeIdentity t = t
-- The following produces a `PartiallyAppliedSynonym` error.
u :: UserNT TypeIdentity
u = UserNT { firstName: "" }
```

## Cause

(Please submit an explanation if you have a good one.)

## Fix

Reconsider how you're using type aliases and data types. Using a type alias as if it is a normal data type may cause problems, as it not their intended purpose.

In the example above, if you want to use the type alias `TypeIdentity`, you will need to apply it to another type alias to produce a concrete type. That concrete type can then be used in a newtype or a function type signature.

```purescript
type UserT' f = { firstName :: f String }
newtype UserNT' = UserNT' (UserT' TypeIdentity)
u :: UserNT'
u = UserNT' { firstName: "test" }
newtype UserNT'Maybe = UserNT'Maybe (UserT' Maybe)
um :: UserNT'Maybe
um = UserNT'Maybe { firstName: Just "test" }
```

## Notes
