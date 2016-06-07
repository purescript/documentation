Code such as `infixl 4 map as <$>` is called a _fixity declaration_, and is made up of four parts:

* The associativity: either `infixl`, `infixr`, or `infix`.
* The precedence: an integer, between 0 and 9. Here, it is 4.
* The function to alias: here, `map`
* The operator: here, `<$>`.

It determines how expressions involving this operator are bracketed.

## Associativity

**Warning**: In most cases, "associativity" refers to the algebraic sense, which is defined as follows: an operator `*` is _associative_ if for all values of `x`, `y`, and `z`, `x * (y * z) = (x * y) * z`. Unfortunately there's a very confusing naming conflict here: in the context of fixity declarations, "associativity" is to do with how your code gets parsed, not its semantics.

`infixl`, or "left-associative", means that repeated applications are bracketed starting from the left. For example, `#` from Prelude is left-associative, meaning that an expression such as:

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

## Precedence

Precedence determines the order in which operators are bracketed. Operators with a higher precedence will be bracketed earlier. For example, take `<$>` and `<#>` from Prelude. `<$>` is precedence 4, whereas `<#>` is precedence 1. If we write:

```
(_ + 1) <$> [1,2,3] <#> (_ * 2)
```

then this is bracketed as follows:

```
((_ + 1) <$> [1,2,3]) <#> (_ * 2)
```

## Default fixity

If you define an operator and don't give it a fixity declaration, then it is assigned the default fixity, which is left-associative with a precedence of -1. This is arguably a bug, since it's not possible to give a precedence of -1 with an explicit declaration. Therefore, this behaviour might well change in a future version of the compiler, and it's probably best to give all your operators explicit fixity declarations to avoid any issues.