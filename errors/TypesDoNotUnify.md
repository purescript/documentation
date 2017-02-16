# `TypesDoNotUnify` Error

## Example

```purescript
> 1 == "test"
Error found:
in module $PSCI
at line 1, column 6 - line 1, column 6

  Could not match type

    String

  with type

    Int   

while checking that type String
  is at least as general as type Int
while checking that expression "test"
  has type Int
```

## Cause

This error occurs when two types are required to be equal, but the type checker is unable to verify that they are equal.

In the example above, the types `String` and `Int` can never be made equal, hence the error.

## Fix

- Look carefully at the error, especially the information at the end. Usually, it will help to narrow down the offending expression. For example, in the error message above, we are told that the error occurred "while checking that expression `"test"` has type `Int`".

## Notes
### Arrays

Unlike in JavaScript, all elements of a PureScript array must be the same type. Otherwise the types will fail unification when being matched by [type inference](https://en.wikipedia.org/wiki/Unification_(computer_science)#Application:_Type_inference). For example, ```[true, false]``` or ```[1, 2, 3]``` work fine but ```[1, false]``` does not.

### Another Example

```purescript
f :: Int -> Int
f x = x + 1

g :: Boolean -> Boolean
g x = x || true

h = g <<< f
```

The type of `(<<<)` (that is, function composition) is

```purescript
forall a b c. (b -> c) -> (a -> b) -> (a -> c)
```

For the right hand side of `h` to type-check, we need to find types `a`, `b`, and `c` such that the types match up. That is, we need to find a choice of `a`, `b`, and `c` such that:

- `b = Boolean` (from the argument type of `g`)
- `c = Boolean` (from the return type of `g`)
- `a = Number` (from the argument type of `f`)
- `b = Number` (from the return type of `f`).

`b` can not be `Boolean` and `Number` at the same time, so this system of equations is not satisfiable, and the type checker rejects the program.

### Matching Labels in Rows

Note that for row types (e.g. in objects or effects) to be equal, not only the types, but the labels themselves must match identically.
