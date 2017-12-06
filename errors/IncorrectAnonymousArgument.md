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

In the case of multiple arguments, give them names:
```purescript
add a b = (a + b)
```
or in the case of a normal function: Write the function [as an operator](https://github.com/purescript/documentation/blob/fc4a9db4b128aa3331e5f990cb1860e59077af31/language/Syntax.md#functions-as-operators) using backticks:
```purescript
mapArray = _ `map` [1, 2, 3]
```


## Notes

- While `_ + _` will give this error; `\a -> a + _` will not

- If you really want to have multiple anonymous arguments, it can be achieved like this:
```purescript
add = (((+) $ _) $ _)
```
