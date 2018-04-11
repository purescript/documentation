# `CannotFindDerivingType` Error

## Example

```purescript
module ShortFailingExample where

type Entry =
  { firstName :: String
  , lastName  :: String
  }

derive instance eqEntry :: Eq Entry
...
```

## Cause

The type declaration for the given type is not in scope.

In the above example a type synonym is declared which cannot be used to declare a type class instance.

## Fix

Instead of using a type synonym we create a new type declaration using the ```newtype``` keyword.

```purescript
module ShortFailingExample where
  
newtype Entry = Entry
  { firstName :: String
  , lastName  :: String
  }

derive instance eqEntry :: Eq Entry
```

## Notes

- Additional notes.
