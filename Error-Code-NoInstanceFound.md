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

This error can also arise as a result of #202: currently, the compiler cannot infer type class constraints. That means that the following will not work:

```purescript
myAdd x y = x + y
```

This fails because `(+)` has a type class constraint, which means that `myAdd` will also need a type class constraint, but the compiler cannot infer the type. The error will look something like this:

```
No instance found for Prelude.Semiring _67
```

The solution is to add an explicit type signature:

```purescript
myAdd :: forall a. (Semiring a) => a -> a -> a
myAdd x y = x + y
```