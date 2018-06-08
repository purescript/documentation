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

A type synonym has been partially applied. To properly use a type synonym having type parameters, like `type B a` or `type TypeIdentity a`, you must apply it to a type parameter at every location in the program, like `B Int` or `TypeIdentity Int`.

To understand the problem, it may be helpful to review the relevant difference between a "type synonym" and a "data type". A type synonym is effectively a macro, a substitution which is performed after parsing into an AST and before type-checking and compilation, while a data type is a proper entity through the compilation process. Because of this, a type synonym has much more restricted use, while a data type enjoys additional flexibility, such as the ability to be partially applied.

Interestingly, a type synonym *can* be partially applied in the context of a type synonym, however. For example:

```purescript
type UserT' f = { firstName :: f String }
u' :: UserT' TypeIdentity
u' = { firstName: "test" }
```

## Fix

Reconsider whether you want to use a type synonym for that particular type declaration, or whether to a data type is more appropriate.

In the example above, if you want to use the type synonym `TypeIdentity`, you will need to apply it to another type synonym to produce a concrete type. That concrete type can then be used in a newtype or a function type signature.

```purescript
type UserT' f = { firstName :: f String }
newtype UserNT' = UserNT' (UserT' TypeIdentity)
u :: UserNT'
u = UserNT' { firstName: "test" }
newtype UserNT'Maybe = UserNT'Maybe (UserT' Maybe)
um :: UserNT'Maybe
um = UserNT'Maybe { firstName: Just "test" }
```

And to fix the second example, we can't simply fully apply `X`, as that would produce a kind error, `Could not match kind Type -> Type with kind Type`. The `f` in `IdentityF f a` needs to be kind `Type -> Type`, but here it is kind `Type`. The most appropriate solution is to simply use `A` or `B` as the type synonym, as demonstrated in `alsoOk` and `alsoAlsoOk`.

## Notes

Eta-expanding makes it impossible to use the synonym in some situations in which it would otherwise work, and unlike some other restrictions we impose on ourselves using the type system, this one is never useful - it only prevents you doing things you might want to do, not things you might want to avoid.

An example, from transformers, if State was defined as follows, it would be impossible to use `State` in some situations.

```
-- note the `a`, in the library this is omitted on both sides
type State s a = StateT s Identity a
````

For example, the following situation is one in which we would want to use `State`. When stacking monad transformers, only the "top" of the stack has its `a`, its second type parameter, provided, and the "lower" layers are used to specify its `m`, its monad. 

```
-- If `State` is defined as above, the following would fail,
--   as the type synonym is only receiving an argument for `s`, the `MyState` type.
newtype App1 a = App1 (ExceptT MyError (State MyState) a)
newtype App2 a = App2 (WriterT MyLog (ExceptT MyError (State MyState)) a)
```

By defining it as `type State s a`, we are saying it requires two type arguments, but in both of the above cases, we apply `MyState` to `State`, which is just one type argument. Because the example didn't include a type for `a` when using the `State`  synonym, it is left partially applied,

Related to this, purescript/purescript#2691 is an issue which requests adding a warning for type synonyms which can be eta-reduced.
