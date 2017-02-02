# `UndefinedTypeVariable` Error

## Example

```purescript
id :: a -> a
id x = x
```

## Cause

This error occurs when an undefined type variable appears in a type signature.

In the example, the type variable `a` is undefined.

Note that PureScript requires all type variables to be defined in type signatures, unlike in Haskell. See also [Differences from Haskell](../language/Differences-from-Haskell.md).

## Fix

- Check the spelling of any type variables.
- A possible fix is to introduce the variable using a `forall` quantifier:

    ```purescript
    id :: forall a. a -> a
    id x = x
    ```

## Notes
