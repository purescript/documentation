"Better go catch it then." - Unhelpful developer.

Skolem constants are types which only unify with themselves, and a limited range of unification variables. They are used to check polymorphic types, usually when using Rank N Types.

We say that a skolem _escapes its scope_ when an attempt is made to unify it with a unification variable which was generated outside the scope in which the skolem was generated.

Consider for example, this function:

```purescript
test = do
  r <- runST (newSTRef 0)
  return 0
```

Here the type of `runST` causes us to check `newSTRef 0` against a polymorphic type, unifying the type of `r` with a skolem constant. However, the type of `r` is fresh, generated outside the scope of `newSTRef 0`, so we see an `EscapedSkolem` error. This is good, because we don't want the reference to leak outside of `runST`.

### `$` and `runST`

One common pitfall is to use the `runST` or `runPure` functions with the `$` operator. This will often lead to the `EscapedSkolem` error due to instantiation of type variables. Avoid this by using parentheses instead of `$`.

### Generalizing Record arguments

Suppose you create a function which validates a password-containing record, like this:

``` purescript
type UserRecord = { username :: String, password :: String }

validatePassword :: UserRecord -> V (Array String) UserRecord
validatePassword rec =
  rec { password = _ } <$> (lengthGreaterThan "Password" 8 rec.password *> pure rec.password)
```

Later, you decide to make this function re-usable, as it should work for *any* record having a password field, so you change the function's type signature to tell the compiler this. Here's your first attempt:

``` purescript
type PasswordRecord = forall r. { password :: String | r}

validatePassword :: PasswordRecord -> V (Array String) PasswordRecord
```

When you compile this, the error you receive will be something like "A type variable has escaped its scope." and mention an "escaped skolem". Why did this fail?

Consider how this polymorphic function specializes using this definition:

``` purescript
validatePassword :: (forall r1. { password :: String | r1}) -> V FormErrors (forall r2. { password :: String | r2})
```

Putting a `forall` in parentheses indicates that a *parameter* is polymorphic, rather than the function. Here, `r1` and `r2` may be specialized to be two different records, when we intended the input and output records to be identical.

To indicate that the function is polymorphic for a given type which is consistent across the function signature, we must ensure the scope of the `forall` spans the entire signature. The following will successfully compile:

``` purescript
validatePassword :: forall r. { password :: String | r} -> V FormErrors { password :: String | r}
```

Let's look at this again. We wanted to use a type synonym for this record, but we failed in our first attempt due to our misunderstanding of how `forall` works. Can we change our type synonym strategy to allow us to use one? We can:

``` purescript
type PasswordRecord r = { password :: String | r }
validatePassword :: forall r. PasswordRecord r -> V FormErrors (PasswordRecord r)
```

We simply remove the `forall` from the type synonym. We can keep the `r` in the type synonym, but its specialized value must be chosen by the caller. The `forall` is moved to the function to indicate the `r` is chosen by the function's caller and must the same `r` in both the function's input and output.

This topic is related to Rank-N Types, for which you can find more reading material. For another example of this, see "Functions as Arguments" below.

### Functions as Arguments

Consider these two functions, `f` and `f'`:

``` purescript
f :: forall a. (a -> a) -> {x :: Int, y :: String}
f g = {x: g 1, y: g "hi"}

f' :: (forall a. a -> a) -> {x :: Int, y :: String}
f' g = {x: g 1, y: g "hi"}
```

Which function's type signature is correctly defined? This function is intended to take one function as an argument and apply it to both an `Int` and a `String` to produce a modified instance of the `{x :: Int, y :: String}` record it encapsulates.

Consider the `f` function first. This indicates that `a` can be specialized when `f` is called.

Consider the `f'` function next. This indicates that the *argument* can be specialized when it is provided.

I know you're curious: which function can we choose to be `g`? It must be able to specialize to become both `Int -> Int` *and* `String -> String`. What function can possibly do that? It's not `show` or `unsafeCoerce` - it's the `id` function, of course.

### Impredicative types

You can also encounter this error when trying to use impredicative polymorphism, e.g. when a type variable should be instantiated with a polymorphic type. An example could be using a polymorphic type in a data structure, or defining a type alias to a universally quantified type and using it as a type parameter. See [GHC docs](https://ghc.haskell.org/trac/ghc/wiki/ImpredicativePolymorphism) or [this tutorial](http://jozefg.bitbucket.org/posts/2014-12-23-impredicative.html) to learn more about the problem.

