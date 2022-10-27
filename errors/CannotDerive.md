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
- For many classes, you can use [Generic][] to obtain the
  instances you want with less effort. For example you can use [Data.Show.Generic][] to derive the Show instance using Generic.

[Generic]: https://pursuit.purescript.org/packages/purescript-prelude/6.0.1/docs/Data.Generic.Rep
[derivable]: https://github.com/purescript/documentation/blob/master/language/Type-Classes.md#type-class-deriving
[Data.Show.Generic]: https://pursuit.purescript.org/packages/purescript-prelude/6.0.1/docs/Data.Show.Generic
