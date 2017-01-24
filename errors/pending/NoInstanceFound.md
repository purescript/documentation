This error occurs when you try to use a function which has a type class constraint with a type (or types) that are not instances of the relevant type class.

Example:

```
> data Foo = Foo
> show Foo
No instance found for Show Foo
```

Here, we use `show`, which is a member of the `Show` type class. Its type is `show :: forall a. (Show a) => a -> String`, which means that `show` takes a value of some type `a` and returns a `String`, with the constraint that `a` must have a `Show` instance.

A possible fix is to add an instance for the relevant type. Following from the earlier example:

```
> instance showFoo :: Show Foo where show Foo = "Foo"
> show Foo
"Foo"
```

This error can also arise in situations where there is no problem with your code, because the compiler is not yet capable of inferring more complex constraints. In these cases, the solution is to add a type signature.

Finally, this error can occur if your code fails to propagate `Partial` constraints properly. For an introduction to the `Partial` type class, please see [The Partial type class](../guides/The-Partial-type-class.md).

---

In the case that this error message is presented when using the `Aff` monad and `liftEff`, be sure to include the kinds of effects your function produces in the type Signature. For instance:

```
logGet :: forall aff. String -> Aff (ajax :: AJAX | aff) Unit
logGet param = do
  res <- get $ baseUrl <> param
  liftEff $ log res.response
```

will throw this error. This is because the type signature is not properly constructed, and should account for the `CONSOLE` effect arising from the `log` function. The type checker will not infer that `CONSOLE` should be a part of this type signature in its warning if the type signature is left out, *probably* due to the lifting of the effect. The correct type signature is:

``` logGet :: forall aff. String -> Aff (ajax :: AJAX, console :: CONSOLE | aff) Unit```


### exhaustivity

This error occurs when a pattern matching definition has **non-exhaustive** patterns.

As an example of this situation, consider the following definition:

```haskell
f :: Number -> Number
f 0 = 0
```

This is clearly not exhaustive, as it fails to cover all the cases for its argument type: if we apply `f` to a value it cannot match (for example `1`) we will get an error at runtime:

```
> f 1

Failed pattern match
```

Such functions are called *partial*, because they are not defined for all inputs: otherwise they are called *total*.

Another example, `Data.Either.Unsafe` exports a function called `fromLeft`:

```haskell
fromLeft :: forall a b. Either a b -> a
fromLeft (Left a) = a
```

The exhaustivity checker will throw the following warning:

```
Warning in module Data.Either.Unsafe:
  Warning in value declaration fromLeft:
  Warning at /home/travis/build/purescript/purescript/core-tests/bower_components/purescript-either/src/Data/Either/Unsafe.purs line 9, column 1 - line 10, column 1:
    Pattern could not be determined to cover all cases.
    The definition has the following uncovered cases:

      (Data.Either.Right _)
```

The solution is to make your functions total in some way. We can use the type `Maybe a` to return `Nothing` in case of a missing case:

```haskell
f_total :: Number -> Maybe Number
f_total 0 = Just 0
f_total _ = Nothing
```

The compiler will not complain to this new definition for `f`.

Up to now, we support exhaustivity checking for Data Constructors, Objects and Literals. If you are keen on using guards, you have to add an `otherwise` or `true` guard case to ensure exhaustivity.

An example with guards:

```haskell
data Nat = Zero | Succ Nat

isZero :: Nat -> Boolean
isZero x | x == Zero = true
         | otherwise = false
```
