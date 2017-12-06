# `IncorrectAnonymousArgument` Error

## Example

```purescript
module Example where

add = (_ + _)

mapArray = map _ [1, 2, 3]
```

## Cause

In an [operator section](https://github.com/purescript/documentation/blob/fc4a9db4b128aa3331e5f990cb1860e59077af31/language/Syntax.md#operator-sections), like `(_ + 1)`, an anonymous argument can be used only once.


## Fix

In the case of multiple arguments:
```purescript
add = \a -> (a + _)
```
or in the case of a normal function: Write the function infix using backticks:
```purescript
mapArray = _ `map` [1, 2, 3]
```


## Notes

If you really want to have multiple anonymous arguments, this can be achieved like this:
```purescript
add = (((+) $ _) $ _)
```
