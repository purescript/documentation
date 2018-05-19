# `PartiallyAppliedSynonym` Error

## Example

```purescript
module ShortFailingExample where

newtype UserNT f = UserNT { firstName :: f String }
type TypeIdentity t = t
-- The following produces a `PartiallyAppliedSynonym` error.
u :: UserNT TypeIdentity
u = UserNT { firstName: "" }

----- Another example

data Const a b = Const a
data IdentityF f a = IdentityF (f a)

ok :: IdentityF (Const Int) Unit
ok = IdentityF (Const 5)

-- A, B, and X are all synonyms for the same thing.
type A = Const

alsoOk :: IdentityF (A Int) Unit
alsoOk = IdentityF (Const 5)

type B a = Const a

alsoAlsoOk :: IdentityF (B Int) Unit
alsoAlsoOk = IdentityF (Const 5)

type X a b = Const a b

-- The X type synonym is partially applied here.
error :: IdentityF (X Int) Unit
error = IdentityF (Const 5)
```

## Cause

A type alias has been partially applied. To properly use a type alias having type parameters, like `type B a` or `type TypeIdentity a`, you must apply it to a type parameter at every location in the program, like `B Int` or `TypeIdentity Int`.

A type synonym *can* be partially applied in the context of a type synonym, however. For example:

```purescript
type UserT' f = { firstName :: f String }
newtype UserNT' = UserNT' (UserT' TypeIdentity)
```

## Fix

Reconsider whether you want to be using a type alias for that particular type declaration. A type synonym is effectively a macro, a substitution which is performed after parsing into an AST and before type-checking and compilation, while a data type exists throughout the compilation process. Because of this, a type synonym has much more restricted use, while a data type enjoys the ability to be partially applied, like a function.

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

And to fix the second example, we can't simply fully apply `X`, as that would produce a kind error, `Could not match kind Type -> Type with kind Type`. The `f` in `IdentityF f a` needs to be kind `Type -> Type`, but here it is kind `Type`. The most appropriate solution is to simply use `A` or `B` as the type alias, as demonstrated in `alsoOk` and `alsoAlsoOk`.

## Notes
