# Values

## Numbers

Numeric literals can be integers (type `Int`) or floating point numbers (type `Number`). Floating point numbers are identified by a decimal point. Integers in hexadecimal notation should be preceded by the characters `0x`:

``` purescript
16 :: Int
0xF0 :: Int
16.0 :: Float
```

## Strings

String literals are enclosed in double-quotes and may extend over multiple lines. Line breaks should be surrounded by slashes as follows:

``` purescript
"Hello World"

"Hello \
\World"
```

Line breaks will be omitted from the string when written this way.

### Triple-quote Strings

If line breaks are required in the output, they can be inserted with `\n`. Alternatively, you can use triple double-quotes to prevent special parsing of escaped symbols. This also allows the use of double quotes within the string with no need to escape them:

``` purescript
jsIsHello :: String
jsIsHello = """
function isHello(greeting) {
  return greeting === "Hello";
}
"""
```

This method of declaring strings is especially useful when writing regular expression strings.

```
regex ".+@.+\\..+" noFlags
regex """.+@.+\..+""" noFlags
```

The example regular expression above is a very simple email address validator. Both are equivalent, but the second one, using triple double-quotes, is much easier to write and maintain. This is even more true when writing complex regular expressions, as many are.

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

## Property Accessors

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

## Record Updates

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

## Operators

Operators in PureScript are just regular functions. The [`Prelude`](https://github.com/purescript/purescript-prelude) and [`Data.Int.Bits`](https://github.com/purescript/purescript-integers) define a number of operators which correspond to JavaScript operators.

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
`mod`      | `%`         | Numeric modulus
`==`       | `==`        | Equality check
`/=`       | `!=`        | Inequality check
`<`        | `<`         | Less than
`<=`       | `<=`        | Less than or equal
`>`        | `>`         | Greater than
`>=`       | `>=`        | Greater than or equal
`&&`       | `&&`        | Boolean AND
`||`       | `||`        | Boolean OR
`.&.`      | `&`         | Binary AND
`.|.`      | `|`         | Binary OR
`.^.`      | `^`         | Binary XOR
`shl`      | `<<`        | Shift Left
`shr`      | `>>`        | Shift Right
`zshr`     | `>>>`       | Zero-fill Shift Right
`<>`       | `+`         | String concatenation

Many of these operators are defined in type classes and work with lots of different types. For example, `+` and `*` work with not only `Int` or `Number`, but any `Semiring` (see "Type classes").

## Operators as values

Operators can be used as normal values by surrounding them with parentheses:

``` purescript
and = (&&)
```

## Operator sections

Binary operators can be partially applied by surrounding them with parentheses and using `_` as one of the operands:

``` purescript
half = (_ / 2)
double = (2 * _)
```

## Functions as operators

Functions can be used as infix operators when they are surrounded by backticks:

``` purescript
foo x y = x * y + y
test = 10 `foo` 20
```

Operator sections also work for functions used this way:

``` purescript
fooBy2 = (_ `foo` 2)
```

## User-defined operators

User-defined infix operators can be defined by providing an operator alias for an existing function.

For example, to create an operator alias for the [`Data.Array.range`](https://github.com/purescript/purescript-arrays) function:

``` purescript
infix 5 Data.Array.range as ..
```

This function can be used as follows::

```purescript
oneToTen = 1 .. 10
```

Operator alias declarations are made up of four parts:

* The associativity: either `infixl`, `infixr`, or `infix`.
* The precedence: an integer, between 0 and 9. Here, it is 5.
* The function to alias: here, `Data.Array.range`
* The operator: here, `..`.

The declaration determines how expressions involving this operator are bracketed.

### Associativity

`infixl` means that repeated applications are bracketed starting from the left. For example, `#` from Prelude is left-associative, meaning that an expression such as:

```
products # filter isInStock # groupBy productCategory # length
```

is bracketed as:

```
((products # filter isInStock) # groupBy productCategory) # length
```

Similarly, `infixr` means "right-associative", and repeated applications are bracketed starting from the right. For example, `$` from Prelude is right-associative, so an expression like this:

```
length $ groupBy productCategory $ filter isInStock $ products
```

is bracketed as:

```
length $ (groupBy productCategory $ (filter isInStock $ products))
```

`infix` means "non-associative", and the parser will bracket up repeated applications in such a way as to minimise the depth of the resulting syntax tree. For example, `==` from Prelude is non-associative, and so:

```
true == false == false == true
```

is bracketed as:

```
(true == false) == (false == true)
```

### Precedence

Precedence determines the order in which operators are bracketed. Operators with a higher precedence will be bracketed earlier. For example, take `<$>` and `<#>` from Prelude. `<$>` is precedence 4, whereas `<#>` is precedence 1. If we write:

```
(_ + 1) <$> [1,2,3] <#> (_ * 2)
```

then this is bracketed as follows:

```
((_ + 1) <$> [1,2,3]) <#> (_ * 2)
```

## If-Then-Else expressions

The `if`, `then` and `else` keywords can be used to create conditional expressions similar to a JavaScript ternary expression. The `else` block is always required:

``` purescript
conditional = if 2 > 1 then "ok" else "oops"
```

## Let and where bindings

The `let` keyword a collection of local declarations, which may be mutually recursive, and which may include type declarations:

``` purescript
factorial :: Int -> Int
factorial =
  let
    go :: Int -> Int -> Int
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

## Do notation

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
Note: (>>=) is the `bind` function for the `Bind` type as defined in the [Prelude package](https://pursuit.purescript.org/packages/purescript-prelude/2.1.0/docs/Prelude#t:Bind).
