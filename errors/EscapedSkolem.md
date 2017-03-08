# `EscapedSkolem` Error

## Example

```purescript
test = do
  r <- runST (newSTRef 0)
  return 0
```

## Cause

Skolem constants are types which only unify with themselves, and a limited range of unification variables. They are used to check polymorphic types, usually when using Rank N Types.

We say that a skolem _escapes its scope_ when an attempt is made to unify it with a unification variable which was generated outside the scope in which the skolem was generated.

In the example above, the type of `runST` causes us to check `newSTRef 0` against a polymorphic type, unifying the type of `r` with a skolem constant. However, the type of `r` is fresh, generated outside the scope of `newSTRef 0`, so we see an `EscapedSkolem` error. This is good, because we don't want the reference to leak outside of `runST`.

## Fix

- If you are using the `ST` effect, you might be leaking a reference to a mutable variable. Carefully inspect the types of any values which might leak outside the call to `runST`, and make sure this is not the case.

## Notes

### `$` and `runST`

One common pitfall is to use the `runST` or `runPure` functions with the `$` operator. This will often lead to the `EscapedSkolem` error due to instantiation of type variables. Avoid this by using parentheses instead of `$`.

### Point free style

In some cases point free style can also leak type variables.

### Impredicative Types

You can also encounter this error when trying to use impredicative polymorphism, e.g. when a type variable should be instantiated with a polymorphic type. An example could be using a polymorphic type in a data structure, or defining a type alias to a universally quantified type and using it as a type parameter. See [GHC docs](https://ghc.haskell.org/trac/ghc/wiki/ImpredicativePolymorphism) or [this tutorial](http://jozefg.bitbucket.org/posts/2014-12-23-impredicative.html) to learn more about the problem.
