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
