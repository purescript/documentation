# `CannotDerive` Error

## Example

```purescript
data Bool = True | False

derive instance showBool :: Show Bool
```

## Cause

This error shows up when you're attempting to derive an instance for a class
for which compiler support does not exist. There is a [fixed list of classes
which can be derived by the compiler][derivable]; for any class not on this
list, however, you'll need to find another way of obtaining an instance.

## Fix

- You will need to write an instance yourself.
- For many classes (including `Show`), you can use [Generics][] to obtain the
  instances you want with less effort.

[Generics]: https://pursuit.purescript.org/packages/purescript-generics-rep
[derivable]: https://github.com/purescript/documentation/blob/master/language/Type-Classes.md#type-class-deriving
