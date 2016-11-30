# Syntax

## Whitespace Rules

Syntax is whitespace sensitive. The general rule of thumb is that declarations which span multiple lines should be indented past the column on which they were first defined on their subsequent lines.

That is, the following is valid:

``` purescript
foo = bar +
  baz
```

But this is not:

``` purescript
foo = bar +
baz
```

## Comments

A single line comment starts with `--`:

``` purescript
-- This is a comment
```

Multi-line comments are enclosed in `{-` and `-}`. These can be nested:

``` purescript
{-
  Comment
  {- nested comment -}
  continued comment
-}
```

Comments that start with a pipe character, `|`, are considered documentation, and will appear in the output of tools like `psc-docs` and Pursuit. For example:

``` purescript
-- | `bool` performs case analysis for the `Boolean` data type, like an `if` statement.
bool :: forall a. Boolean -> a -> a -> a
bool true x _ = x
bool false _ x = x
```

Note that, unlike Haskell, every line which should be considered documentation must start with a pipe. This allows you to do things like:

``` purescript
-- | Sort an array based on its `Ord` instance.
-- |
-- | This implementation runs in `O(n^2)` time, where `n` is the length of the
-- | input array.
-- TODO: try to optimise this?
sort :: forall a. (Ord a) => Array a -> Array a
sort xs = [...]
```

## Top-level declarations

Values at the top level of a module are defined by providing a name followed by an equals sign and then the value to associate:

``` purescript
one = 1
```

Functions can also be defined at the top level by providing a list of patterns on the left hand side of the equals sign:

``` purescript
add x y = x + y
```

See the section on pattern matching for more details about the kinds of patterns that can be used here.

Functions using pattern matching may be defined multiple times to handle different pattern matches:

``` purescript
isEmpty [] = true
isEmpty _ = false
```

This does not mean functions can be arbitrarily overloaded with different numbers or types of arguments though.

Guards can also be used in these definitions:

``` purescript
isEmptyAlt xs | length xs == 0 = true
isEmptyAlt _ = false
```

A top level declaration is generally defined with a type signature:

```purescript
multiply :: Number -> Number -> Number
multiply x y = x * y
```

Type signatures are not required for top-level declarations in general, but is good practice to do so. See the section on types for more details.

## Function application

Function application is indicated by just the juxtaposition of a function with its arguments:

``` purescript
add 10 20
```

PureScript functions are defined as curried, so partial application has no special syntax:

``` purescript
add10 = add 10
```

In fact, `add 10 20` is parsed as `(add 10) 20`.
