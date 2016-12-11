# PureScript language reference

As an introductory example, here is the usual "Hello World" written in PureScript:

```purescript
module Main where

import Control.Monad.Eff.Console

main = log "Hello, World!"
```

## Another Example

The following code defines a `Person` data type and a function to generate a string representation for a `Person`:

```purescript
data Person = Person { name :: String, age :: Int }

showPerson :: Person -> String
showPerson (Person o) = o.name <> ", aged " <> show o.age

examplePerson :: Person
examplePerson = Person { name: "Bonnie", age: 26 }
```

Line by line, this reads as follows:

- `Person` is a data type with one constructor, also called `Person`
- The `Person` constructor takes an object with two properties, `name` which is a `String`, and `age` which is an `Int`
- The `showPerson` function takes a `Person` and returns a `String`
- `showPerson` works by case analysis on its argument, first matching the constructor `Person` and then using string concatenation and object accessors to return its result.
- `examplePerson` is a Person object, made with the `Person` constructor and given the String "Bonnie" for the name value and the Int 26 for the age value.

The full language reference continues below:

1. [Types](Types.md)
2. [Syntax](Syntax.md)
3. [Values](Values.md)
4. [Type Classes](Type-Classes.md)
5. [Pattern Matching](Pattern-Matching.md)
6. [Modules](Modules.md)
7. [FFI](FFI.md)
8. [Records](Records.md)
9. [Differences from Haskell](Differences-from-Haskell.md)
