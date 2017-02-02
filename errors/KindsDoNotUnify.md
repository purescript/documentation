# `KindsDoNotUnify` Error

## Example

```purescript
> import Type.Proxy
> let x = Proxy :: Proxy Array
Error found:
in module $PSCI
at line 1, column 5 - line 1, column 24

  Could not match kind

    Type

  with kind

    Type -> Type


while checking the kind of Proxy Array
```

## Cause

This error occurs when the compiler requires two _kinds_ to be equal, but they are not equal.

In the example above, the type constructor `Proxy` takes an argument of kind `Type`, but `Array` has kind `Type -> Type`, hence the error message.

## Fix

- Look at the information in the error message to find the type with the offending kind.

## Notes

### Additional/Missing Type Arguments

This error can occur when a type argument is missing, or an additional type argument is provided:

```purescript
instance functorArray :: Functor (Array a) where
  ...
```

This results in a kind error, due to the additional type argument `a` passed to `Array` (in `Functor (Array a)`). The solution is to use `Functor Array` instead.
