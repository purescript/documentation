# Frequently Asked Questions

## Why must type class instances be named?

As in `instance arbitraryUnit :: Arbitrary Unit where`.

This has come up several times already
(https://github.com/purescript/purescript/issues/1078,
https://github.com/purescript/purescript/issues/952).
Among the reasons are:

- The instance names are used to help the readability of compiled JavaScript
- Deterministic names are good
  and we don’t have a good function for that which still produces nice names
  - E.g. Combinations of data types and class names
    and duplicate data types in different modules:
    `instance Functor FooBar Baz -> ?`
    `instance Functor Foo BarBaz -> ?`
- Renaming a class or type has the possibility to break working FFI code.
- Possibility to name an instance differently than class or type:
  `instance refl :: TypeEquals a a`


## Why is `:` instead of `=` used for record assignments?

Using `=` would lead to ambiguities:

Does `book {title = "Hello"}` mean "update the title of book to `Hello`",
or does it mean "apply the function `book` to the record `{title = "Hello"}`".

Using `:` avoids this ambiguity.


## Why is `<<<` instead of `.` used for function composition?

`.` is already used to reference record fields as in `book.title`.
