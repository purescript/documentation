# Hello, PureScript!

As an introductory example, here is the usual "Hello World" written in PureScript:

```purescript
module Main where
  
import Control.Monad.Eff.Console
  
main = log "Hello, World!"
```

## Another Example

The following code defines a `Person` data type and a function to generate a string representation for a `Person`:

```purescript
data Person = Person { name :: String, age :: Number }
  
showPerson :: Person -> String
showPerson (Person o) = o.name <> ", aged " <> show o.age
  
examplePerson :: Person
examplePerson = Person { name: "Bonnie", age: 26 }
```

Line by line, this reads as follows:

- ``Person`` is a data type with one constructor, also called ``Person``
- The ``Person`` constructor takes an object with two properties, ``name`` which is a ``String``, and ``age`` which is a ``Number``
- The ``showPerson`` function takes a ``Person`` and returns a ``String``
- ``showPerson`` works by case analysis on its argument, first matching the constructor ``Person`` and then using string concatenation and object accessors to return its result.
- ``examplePerson`` is a Person object, made with the ``Person`` constructor and given the String "Bonnie" for the name value and the Number 26 for the age value.


  



