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

Multi-line comments are enclosed in `{-` and `-}`:

``` purescript
{-
  Comment
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

## Function and Value names

Function and value names must start with a lowercase letter ([unicode category](https://en.wikipedia.org/wiki/Unicode_character_property#General_Category) `Ll`) or underscore `_`. The following characters may be any number of:
* Letter: Unicode category `L`
* Mark: Unicode category `M`
* Number: Unicode category `N`
* Underscore: `_`
* Apostrophe: `'`

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

## Literals

### Numbers

Numeric literals can be integers (type `Int`) or floating point numbers (type `Number`). Floating point numbers are identified by a decimal point. Integers in hexadecimal notation should be preceded by the characters `0x`:

``` purescript
16 :: Int
0xF0 :: Int
16.0 :: Number
```

### Strings

String literals are enclosed in double-quotes and may extend over multiple lines. Line breaks should be surrounded by slashes as follows:

``` purescript
"Hello World"

"Hello \
\World"
```

Line breaks will be omitted from the string when written this way.

#### Triple-quote Strings

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

### Booleans

The boolean literals are `true` and `false`.

### Functions

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

### Arrays

Array literals are surrounded by square brackets, as in JavaScript:

``` purescript
[]
[1, 2, 3]
```

### Records

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

## Additional forms with Records

### Property Accessors

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

These work with any number of levels:

``` purescript
_.nested.property.name
```

### Record Updates

Properties on records can be updated using the following syntax:

``` purescript
rec { key1 = value1, ..., keyN = valueN, nestedKey { subKey = value, ... } }
```

Some or all of the keys may be updated at once, and records inside of records can also be updated.

For example, the following function increments the `foo` property on its argument:

``` purescript
\rec -> rec { foo = rec.foo + 1 }
```

[Nested record updates](https://liamgoodacre.github.io/purescript/records/2017/01/29/nested-record-updates.html) look like this:

``` purescript
r = { val: -1
    , level1: { val: -1
              , level2: { val: -1 }
              }
    }
r' = r { level1 { val = 1 } }
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

## Binary Operators

Operators in PureScript are just regular binary functions. In particular, no operators are built into the language; an overview of operators defined in libraries such as the `Prelude` is therefore outside the scope of this reference.

Operators can be defined by providing an operator alias for an existing function (which must be binary, i.e. its type must be of the form `a -> b -> c`). For example:

``` purescript
data List a = Nil | Cons a (List a)

append :: forall a. List a -> List a -> List a
append xs Nil = xs
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

infixr 5 append as <>
```

This function can be used as follows::

```purescript
oneToThree = Cons 1 (Cons 2 (Cons 3 Nil))
fourToSix = Cons 4 (Cons 5 (Cons 6 Nil))

oneToSix = oneToThree <> fourToSix
```

Operator alias declarations are made up of four parts:

* The associativity: either `infixl`, `infixr`, or `infix`.
* The precedence: an integer, between 0 and 9. Here, it is 5.
* The function to alias: here, `append`
* The operator: here, `<>`.

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

`infix` means "non-associative". Repeated use of a non-associative operator is disallowed. For example, `==` from Prelude is non-associative, which means that

```
true == true == true
```

results in a [NonAssociativeError](../errors/NonAssociativeError.md):

```
Cannot parse an expression that uses multiple instances of the non-associative operator Data.Eq.(==).
Use parentheses to resolve this ambiguity.
```

Non-associative parsing via `infix` is most appropriate for operators `f` for which ``x `f` (y `f` z)`` is not necessarily the same as ``(x `f` y) `f` z)``, that is, operators which do not satisfy the algebraic property of associativity.

### Precedence

Precedence determines the order in which operators are bracketed. Operators with a higher precedence will be bracketed earlier. For example, take `*` and `+` from Prelude. `*` is precedence 7, whereas `+` is precedence 6. Therefore, if we write:

```
2 * 3 + 4
```

then this is bracketed as follows:

```
(2 * 3) + 4
```

Operators of different associativities may appear together as long as they do not have the same precedence. This restriction exists because the compiler is not able to make a sensible choice as to how to bracket such expressions. For example, the operators `==` and `<$>` from the Prelude have the fixities `infix 4` and `infixl 4` respectively, which means that given the expression

```
f <$> x == f <$> y
```

the compiler does not know whether to bracket it as

```
(f <$> x) == (f <$> y)
```

or

```
f <$> (x == f) <$> y
```

Therefore, we get a [MixedAssociativityError](../errors/MixedAssociativityError):

```
Cannot parse an expression that uses operators of the same precedence but mixed associativity:

  Data.Functor.(<$>) is infixl
  Data.Eq.(==) is infix

Use parentheses to resolve this ambiguity.
```

### Operators as values

Operators can be used as normal values by surrounding them with parentheses:

``` purescript
and = (&&)
```

### Operator sections

Operators can be partially applied by surrounding them with parentheses and using `_` as one of the operands:

``` purescript
half = (_ / 2)
double = (2 * _)
```

### Functions as operators

Functions can be used as infix operators when they are surrounded by backticks:

``` purescript
foo x y = x * y + y
test = 10 `foo` 20
```

Operator sections also work for functions used this way:

``` purescript
fooBy2 = (_ `foo` 2)
```

Infix operators created using backticks are left associative with the highest precedence by default.

``` purescript
result = 1 `add` 2 * 3 -- == 9
```

## Case expressions

The `case` and `of` keywords are used to deconstruct values to create logic based on the value's constructors. You can match on multiple values by delimiting them with `,` in the head and cases.

``` purescript
f :: Maybe Boolean -> Either Boolean Boolean -> String
f a b = case a, b of
  Just true, Right true -> "Both true"
  Just true, Left _ -> "Just is true"
  Nothing, Right true -> "Right is true"
  _, _ -> "Both are false"
f (Just true) (Right true)
```

Like top-level declarations, `case` expressions support guards.
``` purescript
f :: Either Int Unit -> String
f x = case x of
  Left x | x == 0 -> "Left zero"
         | x < 0 -> "Left negative"
         | otherwise -> "Left positive"
  Right _ -> "Right"
```

A binding can be avoided by using a single underscore in place of the expression to match on; in this context the underscore represents an _anonymous argument_.
``` purescript
case _ of
  0 -> "None"
  1 -> "One"
  _ -> "Some"
```

This is equivalent to
```purescript
\x -> case x of
  0 -> "None"
  1 -> "One"
  _ -> "Some"
```



## If-Then-Else expressions

The `if`, `then` and `else` keywords can be used to create conditional expressions similar to a JavaScript ternary expression. The `else` block is always required:

``` purescript
conditional = if 2 > 1 then "ok" else "oops"
```

## Let and where bindings

The `let` keyword introduces a collection of local declarations, which may be mutually recursive, and which may include type declarations:

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
factorial :: Int -> Int
factorial = go 1
  where
  go :: Int -> Int -> Int
  go acc 1 = acc
  go acc n = go (acc * n) (n - 1)
```

## Indentation in binding blocks

Indentation of a binding's body is significant. If defining multiple bindings, as in a let-in block, each binding must have the same level of indentation. The body of the binding's definition, then, must be further indented. To illustrate:

``` purescript
f =
  -- The `let-in` block starts at an indentation of 2 spaces, so
  --   the bindings in it must start at an indentation greater than 2.
  let
    -- Because `x` is indented 4 spaces, `y` must also be indented 4 spaces.
    -- Its body, then, must have indentation greater than 4 spaces.
    x :: Int -> Int
    x a =
      -- This body is indented 2 spaces.
      a
    y :: Int -> Int
    y c =
        -- This body is indented 4 spaces.
        c
  in do
    -- Because `m` is indented 4 spaces from the start of the `let`,
    --   `n` must also be indented 4 spaces.
    -- Its body, then, must be greater than 4 spaces.
    let m =
          -- This body is indented 2 spaces.
          x (y 1)
        n =
            -- This body is indented 4 spaces.
            x 1
    log "test"
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
  pure result
```

`maybeSum` takes two values of type ``Maybe Number`` and returns their sum if neither value is `Nothing`.
 
Statements can have the following form:

- `a <- x` which desugars to `bind x \a -> ...`
- `x` which desugars to `bind x \_ -> ...` or just `x` if this is the last statement.
- A let binding `let a = x`. Note the lack of the `in` keyword.

The example `maybeSum` desugars to::

``` purescript
maybeSum a b =
  bind a \n ->
    bind b \m ->
      let result = n + m
      in pure result
```

In practice, you will usually be using [`bind` from the Prelude](https://pursuit.purescript.org/packages/purescript-prelude/docs/Control.Bind#v:bind), but the desugaring will use whichever `bind` is in scope. When using `bind` from the Prelude, there must be an instance of the `Monad` type class for the return type.
