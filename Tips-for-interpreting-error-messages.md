## `_0`...`_n` types

When there is an error message from the type checker that contains a type like `_0` or `_27` or any other number after an underscore, this means the type is an "unknown" - in other words the typechecker has no information at all about it.

This will commonly appear in errors about instances when relying on type inference. Currently the typechecker cannot infer constraints, so the fix is to add an explicit type.

For example:

``` purescript
f a b = a + b
```

Results in:

```
No instance found for Prelude.Semiring _13
```

But if you add an explicit type, all is well:

``` purescript
f :: Number -> Number -> Number
f a b = a + b
```

Adding a type inline can be enough in some situations too, depending on the instance and function being used:

``` purescript
f a b = a + b :: Number
```

Sometimes you do want the polymorphic behaviour that the class allows, in which case you just need to add the constraint manually instead:

``` purescript
f :: forall a. (Semiring a) => a -> a -> a
f a b = a + b
```