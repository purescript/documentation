# `DeclConflict` Error

## Example

```purescript
data MyType = A | B 

data MyType2 = C | D

data MyType3 = MyType3 { x :: Int }

data MyType4 = MyType | MyType2 | MyType3 
```

## Cause

This error shows up when an a defined type (i.e: MyType, MyType2, etc...) is being defined as a data constructor for another data type definition (i.e: MyType4). 

## Fix
This can be fixed by simply wrapping these pre-defined data types in a new data constrcutor.
```purescript
data MyType = A | B 

data MyType2 = C | D

data MyType3 = MyType3 { x :: Int }

data MyType4 = T MyType | T2 MyType2 | T3 MyType3 
```

