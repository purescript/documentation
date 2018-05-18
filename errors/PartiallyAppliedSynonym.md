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

A type alias has been replaced with its value, and the result is that an expression has been partially applied.

In the example above, the data type `UserNT` is applied to the type alias `TypeIdentity t`. In the compiler, type aliases are replaced with the underlying type before type-checking, so the type-checker sees a type `UserNT` being applied to a type `t`.

## Fix

Reconsider how you're using type aliases and data types. Using a type aliase as if it is a normal data type may cause problems, as it not their intended purpose.

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
