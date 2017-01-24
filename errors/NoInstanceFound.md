# `NoInstanceFound` Error

## Example

```
> data Foo = Foo
> show Foo
No instance found for Show Foo
```

## Cause

This error occurs when you try to use a function which has a type class constraint with a type (or types) that are not instances of the relevant type class.

In the example above, we use `show`, which is a member of the `Show` type class. Its type is

```purescript
show :: forall a. Show a => a -> String
```

This means that `show` takes a value of some type `a` and returns a `String`, with the constraint that `a` must have a `Show` instance.

This error can also arise in situations where the compiler is not able to solve a constraint involving ambiguous types. For example:

```purescript
bad = show mempty
```

Finally, this error can occur if your code fails to propagate `Partial` constraints properly. For an introduction to the `Partial` type class, please see [The Partial type class](../guides/The-Partial-type-class.md) or the note [below](#Exhaustivity-Errors).

## Fix

- A possible fix is to add an instance for the relevant type. Following from the earlier example:

    ```
    > instance showFoo :: Show Foo where show Foo = "Foo"
    > show Foo
    "Foo"
    ```

- If a constraint involves an ambiguous type, consider adding a type signature:

    ```
    better = show (mempty :: String)
    ```

## Notes

### Exhaustivity Errors

The `NoInstanceFound` error occurs when a pattern matching definition has **non-exhaustive** patterns.

As an example of this situation, consider the following definition:

```
> let f 0 = 0
```

This function does not handle all possible inputs: it is undefined for all inputs other than zero. Such functions are called *partial* and the compiler will infer a `Partial` constraint:

```
> :type f
(Partial) => Int -> Int
```

If we try to use the function directly, we will get a `NoInstanceFound` error:

```
> f 1

A case expression could not be determined to cover all inputs.
The following additional cases are required to cover all inputs:

  _

Alternatively, add a Partial constraint to the type of the enclosing value.
```

This error can be removed by modifying the definition of `f` to handle all possible inputs.
