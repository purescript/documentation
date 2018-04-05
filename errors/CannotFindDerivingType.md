# `CannotFindDerivingType` Error

## Example

```purescript
module ShortFailingExample where

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

derive instance eqEntry :: Eq Entry
...
```

## Cause

The type declaration for the given type is not in scope.

In the above example a type synonym is declared which cannot be used to declare a type class instance.

## Fix

Instead of using a type synonym we create a new type declaration using the ```newtype``` keyword.

Typeclass declarations can now be made when a type defined using either ```newtype``` or ```data``` is in scope.

```purescript
module ShortFailingExample where
  
newtype Entry = Entry
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

derive instance eqEntry :: Eq Entry
```

## Notes

- Additional notes.
