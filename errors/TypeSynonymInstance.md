# `TypeSynonymInstance` Error

## Example

```purescript
module TypeSynonymInstanceError where
import Prelude

type Mass = Number

instance showMass :: Show Mass where
  show mass = "Mass: " <> show mass <> "kg"
```

## Cause

*Type synonyms* are merely aliases (usually created to avoid verbose type annotations) and do not declare a new unique type. It is still possible to use the synonym and its equivalent type interchangeably:

```purescript
velocity = 123.456 :: Mass  -- oops
```

Since every value that structually matches the type synonym could be considered of the same type, it is not allowed to implement type classes based on type synonyms.

## Fix

If a type is wrapped in a **newtype** instead, it becomes a different type than the underlying type. Newtypes can be used as type class instances:

```purescript
newtype Mass = Mass Number

instance showMass :: Show Mass where
  show (Mass mass) = "Mass: " <> show mass <> "kg"
```

## Notes

The same error occurs when the aliased type is a record:

```purescript
type Point = { x :: Number, y :: Number }

instance showPoint :: Show Point where
  show p = "Point " <> show p.x <> ", " <> show p.y
```
