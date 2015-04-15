You can control what gets exported from a module by using an export list. When an export list is used, other modules will only be able to see things which are in the export list. For example:

```purescript
module Test (exported) where

exported :: Number -> Number
exported = (+1) <<< notExported

notExported :: Number -> Number
notExported = (*3)
```

In this case, modules importing `Test` will not be able to see the `notExported` function.

## Exporting values

To export a value, simply add it to the list. For example, if your module declares values `a`, `b`, and `c`:

```purescript
module Test (a, b, c) where
```

## Exporting data types

To export a data type, add it to the list. Constructors may be exported individually by putting them inside brackets after the type, or you can export all constructors by using `(..)`. For example, suppose we have a declaration in our module as follows: `data Maybe a = Just a | Nothing`.

* `module Data.Maybe (Maybe) where` exports `Maybe`, but not `Just` or `Nothing`.
* `module Data.Maybe (Maybe(Just)) where` exports `Maybe` and `Just`, but not `Nothing`.
* `module Data.Maybe (Maybe(..)) where` exports `Maybe` and both of its constructors.

## Exporting type classes

To export a type class, simply add it to the list, together with all of its members. Unfortunately there is no short-hand for exporting all type class members in one go.

For example, suppose we have the following:

```purescript
class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> (f a) -> b
  foldl :: forall a b. (b -> a -> b) -> b -> (f a) -> b
  foldMap :: forall a m. (Monoid m) => (a -> m) -> (f a) -> m
```

Then you can export the class with:

```purescript
module Test (Foldable, foldr, foldl, foldMap) where
```

If a type class is exported, then all of its members must also be exported. Likewise, if a type class member is exported, the type class it belongs to must also be exported.