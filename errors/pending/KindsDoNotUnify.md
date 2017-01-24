# Cannot unify kind * with kind # *
This can happen when a function is declared without kind `*`, like a function that attempts to return a row:

```purs
onClick :: forall a. String -> (click :: String | a)
onClick s = {click: s}
```

The error message reads along the lines of:
```purs
  Prim.String -> (click :: Prim.String | a)

  Cannot unify kind
    *
  with kind
    # *
```

**Prime suspect:** You are missing an `->` in your type signature.


# Cannot unify kind * with kind * -> *
This error occurs when you try adding a type class instance with a superfluous type parameter, e.g. (from the Purescript by Example book):
```purs
data NonEmpty a = NonEmpty a [a]

instance functorNonEmpty :: Functor (NonEmpty a) where
  (<$>) f (NonEmpty x xs) = NonEmpty (f x) []
```
The correct version is (just use `Functor NonEmpty` instead of `Functor (NonEmpty a)`):
```purs
instance functorNonEmpty :: Functor NonEmpty where
  (<$>) f (NonEmpty x xs) = NonEmpty (f x) []
```

To further illustrate why the type parameter is superfluous, we can compare the `Show` and `Functor` type classes.  Note, that the `Show` type class accepts a _type_ `a`, and the `show` function must accept a value of that type as the first parameter.

```purs
class Show a where
  show :: a -> String
```

This is different from the `Functor` type class which accepts a _type constructor_ `f`, which is used to create a new type `f a` and `f b`.

```purs
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
```
