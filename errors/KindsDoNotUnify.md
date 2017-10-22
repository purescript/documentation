# `KindsDoNotUnify` Error

## Example

```purescript
> import Type.Proxy
> x = Proxy :: Proxy Array
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

### Order of Error Message

In the `Proxy Array` example above it is unclear if `Type` refers to the expected kind and `Type -> Type` refers to the actual kind found on line 1 or vice-versa.  The current implementation of Purescript sometimes presents the expected value first and other times presents the actual value first.  In this sense the presentation of this error is inconsistent, so please don't read any order into the error message.

### Additional/Missing Type Arguments

This error can occur when a type argument is missing, or an additional type argument is provided:

```purescript
instance functorArray :: Functor (Array a) where
  ...
```

This results in a kind error, due to the additional type argument `a` passed to `Array` (in `Functor (Array a)`). The solution is to use `Functor Array` instead.
