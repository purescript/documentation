# `NoInstanceFound` Error

## Example

```
> data Foo = Foo
> show Foo
No type class instance found for Data.Show.Show Foo

> data Fooy a = Fooy a
> data Bar a = Bar (Fooy a)
> derive instance functorBar :: Functor Bar
No type class instance was found for Data.Functor.Functor Fooy
```

## Causes and Fixes

This error occurs when the type-checker can't find an instance of a type class for a data type where it expects or requires one.

Some situations in which this can occur:

### Can't find instance

This error is caused when a function has a type signature having a type class constraint on a type, but the type-checker can't find an instance of the type class for that type.

In the `show Foo` example above, we use `show`, which is a member of the `Show` type class. Its type is

```purescript
show :: forall a. Show a => a -> String
```

This means that `show` takes a value of some type `a` and returns a `String`, with the constraint that `a` must have a `Show` instance. The "NoInstanceFound" error will arise if the compiler can't find a `Show` instance for the `Foo` data type.

This can be fixed by adding an instance for the relevant type. To fix the earlier example:

```
> instance showFoo :: Show Foo where show Foo = "Foo"
> show Foo
"Foo"
```

### Ambiguous types
 
This error can arise in situations where the compiler is not able to solve a constraint due to ambiguous types. This is demonstrated in the following example in which no concrete type appears in the function's definition; it only uses type class methods. The compiler must be given a concrete type, or be able to infer it, to choose a type class instance.

```purescript
bad = show mempty
```

To fix this, consider adding a type signature:

```purescript
better = show (mempty :: String)
```

### Partial type class & non-exhaustive patterns

This error can occur if your code fails to propagate `Partial` constraints properly. For an introduction to the `Partial` type class, please see the [purescript-partial library](https://pursuit.purescript.org/packages/purescript-partial).

As an example of this situation, consider the following function definition:

```
> f 0 = 0
```

This function does not handle all possible inputs: it is undefined for all inputs other than zero. Such functions are called *partial* and the compiler will infer a `Partial` constraint:

```
> :type f
(Partial) => Int -> Int
```

If we try to use the function directly, we will get a `NoInstanceFound` error:

```
> g = f 1

A case expression could not be determined to cover all inputs.
The following additional cases are required to cover all inputs:

  _

Alternatively, add a Partial constraint to the type of the enclosing value.
```

This error can be removed by modifying the definition of `f` to handle all possible inputs.
