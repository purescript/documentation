# `InvalidNewtypeInstance` Error

## Example

```purescript
newtype ManyErrors e a = ManyErrors (Either e (Maybe a))

derive newtype instance manyErrorsFunctor :: Functor (ManyErrors e)
```

## Cause

While `ManyErrors` is a valid newtype, its underlying representation is not a
type for which a `Functor` instance already exists. Most causes of this error are
due to the newtype being some composition of other types (in our case, `Either`
and `Maybe`) instead of just one type.

## Fix

- In many cases you can use existing higher-order newtypes that already have the
relevant instances defined. For example, we could have done the following without
getting an error:

```purescript
import Data.Functor.Compose (Compose)

newtype ManyErrors e a = ManyErrors (Compose (Either e) Maybe a)

derive newtype instance manyErrorsFunctor :: Functor (ManyErrors e)
```

This works because `Compose f g` is a functor whenever `f` and `g` are functors.

Otherwise, we would need to explicitly write the instance ourselves.

## Notes

- If the underlying type does not have an instance to derive, you will instead
get a [NoInstanceFound](https://github.com/purescript/documentation/blob/master/errors/NoInstanceFound.md)
error.
