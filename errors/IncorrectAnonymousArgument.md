# `IncorrectAnonymousArgument` Error

## Example

```purescript
module Example where

sub3 = _ - 3

square = (_ * _)
result1 = map (_ * _) [1, 2, 3]

result2 = map (div _ 2) [1, 2, 3]

result3 = map (1 + _ / 2) [1, 2, 3]
```

## Cause

An [operator section](https://github.com/purescript/documentation/blob/master/language/Syntax.md#operator-sections), like `(_ + 1)`, has the following requirements:
- The expression must be bracketed (surrounded by parens).
- An anonymous argument can be used only once.
- The anonymous argument must neighbor a [binary operator](https://github.com/purescript/documentation/blob/master/language/Syntax.md#binary-operators).
- The anonymous argument must be the _sole expression_ for one of the binary operator arguments.

## Fix

Surround with parens:
```purescript
sub3 = (_ - 3)
```

Replace multiple anonymous arguments with named arguments: 
```purescript
square a = a * a
result1 = map (\n -> n * n) [1, 2, 3]
```

Write the function [as an operator](https://github.com/purescript/documentation/blob/master/language/Syntax.md#functions-as-operators) using backticks:
```purescript
result2 = map (_ `div` 2) [1, 2, 3]
```
or simply use the corresponding infix operator if one is available:
```purescript
result2 = map (div _ 2) [1, 2, 3]
```

Replace the anonymous argument with a named argument: 
```purescript
result3 = map (\n -> 1 + n / 2) [1, 2, 3]
```

## Notes

- While `(_ + _)` will give this error; `\a -> (a + _)` will not

- If you really want to have multiple anonymous arguments, it can be achieved like this:
```purescript
add = (((+) $ _) $ _)
```
