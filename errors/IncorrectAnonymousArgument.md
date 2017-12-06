# `IncorrectAnonymousArgument` Error

## Example

```purescript
module Example where

add = (_ + _)

mapArray = map _ [1, 2, 3]
```

## Cause

Anonymous Arguments are only allowed once in an expression and only with functions written in infix notation.


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
