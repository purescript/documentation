# Whitespace Rules

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

# Comments

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

# Top-level declarations

Values at the top level of a module are defined by providing a name followed by an equals sign and then the value to associate:

``` purescript
one = 1
```

Functions can also be defined at the top level by providing a list of patterns on the left hand side of the equals sign:

``` purescript
add x y = x + y
```

See [[the page on pattern matching|Language Guide: Pattern Matching]] for more details about the kinds of patterns that can be used here.

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

``` purescript
multiply :: Number -> Number -> Number
multiply x y = x * y
```

Type signatures are not required for top-level declarations in general, but is good practice to do so. See [[the page on types for more details|Language Guide: Types]].

# Function application

Function application is indicated by just the juxtaposition of a function with its arguments:

``` purescript
add 10 20
```

PureScript functions are defined as curried, so partial application has no special syntax:

``` purescript
add10 = add 10
```

# Values

## Numbers

Numeric literals can be integers or floating point numbers. Numbers in hexadecimal notation should be preceded by the characters `0x`:

``` purescript
16
16.0
0xF0
```

## Strings

String literals are enclosed in double-quotes and may extend over multiple lines. Line breaks should be surrounded by slashes as follows:

``` purescript
"Hello World"

"Hello \
\World"
```

Line breaks will be ommitted from the string when written this way. If line breaks are required in the output, they can be inserted with `\n`, or by using an alternate string syntax, where the string is enclosed in triple double-quotes. This also allows the use of double quotes within the string with no need to escape them, this is commonly used when making definitions for the FFI:

``` purescript
"""
function isHello(greeting) {
  return greeting === "Hello";
}
"""
```

## Booleans

The boolean literals are `true` and `false`.

## Functions

Function values (sometimes called _lambdas_) are introduced by using a backslash followed by a list of argument names:

``` purescript
\a b -> a + b
```

which would correspond to the following JavaScript:

``` javascript
function (a) {
  return function (b) {
    return a + b;
  }
}
```

## Arrays

Array literals are surrounded by square brackets, as in JavaScript:

``` purescript
[]
[1, 2, 3]
```

### Array indexing

The `Prelude.Unsafe` module defines the `unsafeIndex` function which retrieves the element of an array at an index:

``` purescript
head xs = unsafeIndex xs 0
```

The code generator will turn the expression `unsafeIndex arr index` into the simplified JavaScript `arr[index]`.

The [`purescript-arrays`](https://github.com/purescript/purescript-arrays) library defines an alternative safe version, `index`, also available as infix `!!`, which checks arrays bounds and returns a value of type `Maybe a`:

``` purescript
safeHead xs = xs !! 0
```

## Records

Record literals are surrounded by braces, as in JavaScript:

``` purescript
{}
{ foo: "Foo", bar: 1 }
```

Record literals with wildcards can be used to create a function that produces the record instead:

``` purescript
{ foo: _, bar: _ }
```

is equivalent to:

``` purescript
\foo bar -> { foo: foo, bar: bar }
```

### Property accessors

To access a property of a record, use a dot followed by the property name, as in JavaScript:

``` purescript
rec.propertyName
```

There are also partially applied accessors, where an underscore is followed by a property name:

``` purescript
_.propertyName
```

This is equivalent to:

``` purescript
\rec -> rec.propertyName
```

### Record updates

Properties on records can be updated using the following syntax:

``` purescript
rec { key1 = value1, ..., keyN = valueN }
```

For example, the following function increments the `foo` property on its argument:

``` purescript
\rec -> rec { foo = rec.foo + 1 }
```

Wildcards can also be used in updates to produce a partially applied update:

``` purescript
rec { foo = _ }
```

This is equivalent to:

``` purescript
\foo -> rec { foo = foo }
```

An underscore can also appear in the object position in an updater:

``` purescript
_ { foo = 1 }
```

This is equivalent to:

``` purescript
\rec -> rec { foo = 1 }
```

# Operators

Operators in PureScript are just regular functions. The [`Prelude`](https://github.com/purescript/purescript/tree/master/prelude) defines a number of operators which correspond to JavaScript operators.

## Unary operators

Function     | JS Operator | Meaning
------------ | ----------- | ----------------
`negate`     |  `-`        | Numeric negation
`not`        |  `!`        | Boolean negation
`complement` |  `~`        | Binary negation

## Binary operators

Function   | JS Operator | Meaning
---------- | ----------- | ----------------------
`+`        | `+`         | Numeric addition
`-`        | `-`         | Numeric subtraction
`*`        | `*`         | Numeric multiplication
`/`        | `/`         | Numeric division
`%`        | `%`         | Numeric modulus
`==`       | `==`        | Equality check
`/=`       | `!=`        | Inequality check
`<`        | `<`         | Less than
`<=`       | `<=`        | Less than or equal
`>`        | `>`         | Greater than
`>=`       | `>=`        | Greater than or equal
`&&`       | `&&`        | Boolean AND
`||`       | `||`        | Boolean OR
`&`        | `&`         | Binary AND
`|`        | `|`         | Binary OR
`^`        | `^`         | Binary XOR
`shl`      | `<<`        | Shift Left
`shr`      | `>>`        | Shift Right
`zshr`     | `>>>`       | Zero-fill Shift Right
`++`       | `+`         | String concatenation*

_* PureScript's `++` is an operator for `Semigroup`s in general, not just strings._

## Operators as values

Operators can be used as normal values by surrounding them with parentheses:

``` purescript
and = (&&)
```

## Operator sections

Binary operators can be partially applied by listing them with one operand surrounded by parentheses:

``` purescript
half = (/ 2)
double = (2 *)
```

## Functions as operators

Functions can be used as infix operators when they are surrounded by backticks:

``` purescript
foo x y = x * y + y
test = 10 `foo` 20
```

Operator sections also work for functions used this way:

``` purescript
fooBy2 = (`foo` 2)
```

## User-defined operators

User-defined infix operators can be defined by enclosing names in parentheses.

For example, to create an infix synonym for the [`Data.Array.range`](https://github.com/purescript/purescript-arrays) function:

``` purescript
(..) = Data.Array.range
```

This function can be used as follows::

```
oneToTen = 1 .. 10
```

The associativity and precedence level of operators can be defined with the `infix` (no associativity), `infixl` (left associative), and `infixr` (right associative) top-level declarations:

```
infix 5 ..
infixl 7 %%
infixr 9 ^^
```

# If-Then-Else expressions

The `if`, `then` and `else` keywords can be used to create conditional expressions similar to a JavaScript ternary expression. The `else` block is always required:

``` purescript
conditional = if 2 > 1 then "ok" else "oops"
```

# Let and where bindings

The `let` keyword a collection of local declarations, which may be mutually recursive, and which may include type declarations::

``` purescript
factorial :: Number -> Number
factorial =
  let
    go :: Number -> Number -> Number
    go acc 1 = acc
    go acc n = go (acc * n) (n - 1)
  in
    go 1
```

The `where` keyword can also be used to introduce local declarations at the end of a value declaration:

``` purescript
factorial :: Number -> Number
factorial = go 1
  where
  go :: Number -> Number -> Number
  go acc 1 = acc
  go acc n = go (acc * n) (n - 1)
```

# Do notation

The `do` keyword introduces simple syntactic sugar for monadic expressions.

Here is an example, using the monad for the [`Maybe`](https://github.com/purescript/purescript-maybe) type:

``` purescript
maybeSum :: Maybe Number -> Maybe Number -> Maybe Number
maybeSum a b = do
  n <- a
  m <- b
  let result = n + m
  return result
```

`maybeSum` takes two values of type ``Maybe Number`` and returns their sum if neither number is `Nothing`.

When using `do` notation, there must be a corresponding instance of the `Monad` type class for the return type.

Statements can have the following form:

- `a <- x` which desugars to `x >>= \a -> ...`
- `x` which desugars to `x >>= \_ -> ...` or just `x` if this is the last statement.
- A let binding `let a = x`. Note the lack of the `in` keyword.

The example `maybeSum` desugars to::

``` purescript
maybeSum a b =
  a >>= \n ->
    b >>= \m ->
      let result = n + m
      in return result
```
