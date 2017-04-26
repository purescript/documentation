---
title: Generic Programming
author: Phil Freeman
published: 2017-04-25
---

The PureScript compiler has supported _generic deriving_ in some form since version 0.7.3, thanks to [Gershom Bazerman](http://gbaz.github.io/). However, since then, it has gone through several iterations, and the current version (implemented in the `purescript-generics-rep` library) makes it possible to have the compiler derive a wide variety of boilerplate code based on types. One example of this is the serialization and deserialization of JSON, and in this post I'll show how to use the [`purescript-foreign-generic`](https://github.com/paf31/purescript-foreign-generic) library to create such functions using generics.

#### Generics Overview

PureScript's generics are supported by the `purescript-generics-rep` library, and in particular, the `Data.Generic.Rep.Generic` type class:

```purescript
class Generic a rep | a -> rep where
  from :: a -> rep
  to :: rep -> a
```

There are three interesting things here:

- The `rep` type argument and associated functional dependency define a type function from user types (`a`) to their _representations_ (`rep`).
- `from` converts a regular value into the representation type.
- `to` converts a representation value back into a regular value.

`purescript-generics-rep` provides standard representation types which can be used to represent any `data` types which can be expressed in PureScript code.

It is possible to write out `Generic` instances for our own types by hand, but doing so is very laborious. Instead, we can _derive_ instances by using the `derive` keyword:

```purescript
newtype Person = Person { name :: String, location :: String }

derive instance genericPerson :: Generic Person _
```

Note that the second type argument, which represents the representation type, is specified as a type wildcard. This is useful, because representation types can get quite large, so it is inconvenient to type them out by hand in deriving declarations.

#### Show, Eq, Ord

The key insight regarding generics is this: if we can write a function which works with any of the standard representation types, then we implement the same function for any instance of `Generic`. We can even exploit type information in our implementation by using additional type classes to reflect the type information at runtime.

`purescript-generics-rep` provides helper functions for implementing common type classes from the Prelude:

- `genericShow` gives a default implementation of `show` from the `Show` class
- `genericEq` gives a default implementation of `eq` from the `Eq` class
- `genericCompare` gives a default implementation of `compare` from the `Ord` class
- `genericAppend` gives a default implementation of `append` from the `Semigroup` class
- `genericMempty` gives a default implementation of `mempty` from the `Monoid` class

Using these functions is as simple as dropping the generic implementation into your instances:

```purescript
instance showPerson :: Show Person where
  show = genericShow

instance eqPerson :: Eq Person where
  eq = genericEq

instance ordPerson :: Ord Person where
  compare = genericCompare

instance semigroupPerson :: Semigroup Person where
  append = genericAppend
```

#### Handling Foreign Data

The `purescript-foreign` library is used in PureScript to handle untrusted external data, and to turn such data into typed values. This functionality is represented by the `Decode` type class in the `purescript-foreign-generic` library:

```purescript
class Decode a where
  decode :: Foreign -> F a
```

`Decode` instances are a good example of boilerplate code. In most cases, we proceed based on the structure of the type in question. For example, here is one possible implementation of `Decode` for our `Person` type, using the new _field puns_ feature:

``` haskell
instance decodePerson :: Decode Person where
  decode value = do
    name     <- value ! "name"
    location <- value ! "location"
    return $ Person { name, location }
```

This is not too bad, but real-world records often contain many more fields. Also, it would be nice if we could be sure that the corresponding _encoding_ function would always generate compatible data. Let's see how to verify the same data using `Generic`, which will solve both of these problems.

The `purescript-foreign-generic` library defines a function `genericDecode`, with the following type:

``` haskell
genericDecode 
  :: forall a rep
   . Generic a rep
  => GenericDecode rep
  => Options
  -> Foreign
  -> F a
```

The `Options` type here is based on the options record from Haskell's `aeson` library. For our purposes, the default options will work, but we need to turn on the `unwrapSingleConstructors` option, so that our `newtype` constructor gets ignored during serialization:

``` haskell
myOptions :: Options
myOptions = defaultOptions { unwrapSingleConstructors = true }
```

With this, our `Decode` instance is as simple as:

``` haskell
instance decodePerson :: Decode Person where
  decode = genericDecode myOptions
```

We can test out this instance in PSCi as follows:

```text
> import Data.Generic.Rep
> import Data.Generic.Rep.Show

> map genericShow (decodeJSON "{ 'name': 'John Smith', 'location': 'USA' }" :: Either ForeignError Person)
Right (Person { name: "John Smith", location: "USA" })
```

#### Generating JSON

Just as `genericDecode` can be used to _read_ well-typed data, the `genericEncode` and `genericEncodeJSON` functions can be used to produce the appropriate data or JSON from a PureScript value. The generated `genericDecode` and `genericEncode` functions are inverse to each other for any given input type.

In PSCi, we can test JSON generation for our `Person` data type:

```text
> encodeJSON (Person { name: "John Smith", location: "USA" })
"{ 'name': 'John Smith', location: 'USA' }"
```

One application of this technique is to produce and consume JSON for use with JSON web services, using generics to reduce the amount of boilerplate model code needed.

#### Performance Concerns

Generic deriving can be very convenient for code generation, but it comes with a performance penalty. Consider defining a `Show` instance using `genericShow` - instead of simply converting our data type directly to a `String`, we first convert it to the representation type, and then convert that representation into a `String`. Creating this intermediate structure comes with a cost.

Thankfully, the `generics-rep` approach means that we only need to perform a shallow copy of the data, up to the first data constructor or record, so in practice the performance cost is acceptable. In the case of `foreign-generic`, the benefits listed above usually outweight the performance cost, since we rarely need to parse or generate JSON in performance-critical sections of code in many applications.

