---
title: Generic Programming
author: Phil Freeman
published: 2015-10-20
---

As of version 0.7.3, the PureScript compiler now supports _generic deriving_, thanks to [Gershom Bazerman](http://gbaz.github.io/). Generic deriving makes it possible to have the compiler derive boilerplate code based on types. One example of this is the serialization and deserialization of JSON, and in this post I'll show how to use the [`purescript-foreign-generic`](https://github.com/paf31/purescript-foreign-generic) library to create such functions using generics.

#### Generics Overview

PureScript's generics are supported by the `purescript-generics` library, and in particular, the `Data.Generic.Generic` type class:

``` haskell
class Generic a where
  toSignature :: Proxy a -> GenericSignature
  toSpine     :: a -> GenericSpine
  fromSpine   :: GenericSpine -> Maybe a
```

`Generic` defines three functions:

- `toSignature` creates a generic _signature_ for a type. We can think of this as a representation of a type at runtime.
- `toSpine` converts a value into a generic _spine_, which is a common representation for many types of data.
- `fromSpine` converts a spine back into a value.

`purescript-generics` provides `Generic` instances for lots of types in PureScript's standard libraries.

It is possible to write out instances for our own types by hand, but doing so is very laborious. Instead, we can _derive_ instances by using the `derive` keyword:

``` haskell
newtype Person = Person { name :: String, location :: String }

derive instance genericPerson :: Generic Person
```

#### Show, Eq, Ord

The key insight regarding generics is this: if we can write a function which works with any `GenericSpine`, then we implement the same function for any instance of `Generic`. We can even exploit type information in our implementation by using `toSignature` to reflect the type information at runtime.

`purescript-generics` provides helper functions for implementing common type classes from the Prelude:

- `gShow` gives a default implementation of `show` from the `Show` class
- `gEq` gives a default implementation of `eq` from the `Eq` class
- `gCompare` gives a default implementation of `compare` from the `Ord` class

Using these functions is as simple as dropping the generic implementation into your instances:

``` haskell
instance showPerson :: Show Person where
  show = gShow

instance eqPerson :: Eq Person where
  eq = gEq

instance ordPerson :: Ord Person where
  compare = gCompare
```

#### Handling Foreign Data

The `purescript-foreign` library is used in PureScript to handle untrusted external data, and to turn such data into typed values. This functionality is represented by the `IsForeign` type class:

``` haskell
class IsForeign a where
  read :: Foreign -> Either ForeignError a
```

`IsForeign` instances are a good example of boilerplate code. In most cases, we proceed based on the structure of the type in question. For example, here is one possible implementation of `IsForeign` for our `Person` type, using the new _field puns_ feature:

``` haskell
instance isForeignPerson :: IsForeign Person where
  read value = do
    name <- readProp "name" value
    location <- readProp "location" value
    return $ Person { name, location }
```

This is not too bad, but real-world records often contain many more fields. Let's see how to verify the same data using `Generic`.

The `purescript-foreign-generic` library defines a function `readGeneric`, with the following type:

``` haskell
readGeneric :: forall a. Generic a => Options -> Foreign -> F a
```

The `Options` type here is based on the options record from Haskell's `aeson` library. For our purposes, the default options will work, but we need to turn on the `unwrapNewtypes` option, so that our `newtype` constructor gets ignored during serialization:

``` haskell
myOptions :: Options
myOptions = defaultOptions { unwrapNewtypes = true }
```

With this, our `IsForeign` instance is as simple as:

``` haskell
instance isForeignPerson :: IsForeign Person where
  read = readGeneric myOptions
```

We can test out this instance in PSCi as follows:

```text
> import Data.Generic
> import Data.Foreign.Class

> map gShow (readJSON "{ 'name': 'John Smith', 'location': 'USA' }" :: Either ForeignError Person)
Right (Person { name: "John Smith", location: "USA" })
```

#### Handling Null

The default `Options` object also enables the `maybeAsNull` option, which special-cases the `Maybe` data type for handling null and undefined values.

For example, if we want to allow the `location` property to be nullable in the `Person` data type, we can simply change our type as follows:

``` haskell
newtype Person = Person { name :: String, location :: Maybe String }
```

Our `IsForeign` implementation will be updated to handle null/undefined values automatically. For example, back in PSCi:

```text
> map gShow (readJSON "{ 'name': 'John Smith' }" :: Either ForeignError Person)
Right (Person { name: "John Smith", location: Nothing })
```

#### Generating JSON

Just as `readGeneric` can be used to _read_ well-typed data, the `toForeignGeneric` and `toJSONGeneric` functions can be used to produce the appropriate data or JSON from a PureScript value. The generated `readGeneric` and `toForeignGeneric` functions are inverse to each other for any given input type.

In PSCi, we can test JSON generation for our `Person` data type:

```text
> toJSONGeneric (Person { name: "John Smith", location: Nothing })
"{ 'name': 'John Smith', location: null }"
```

One application of this technique is to produce and consume JSON for use with JSON web services, using generics to reduce the amount of boilerplate model code needed.

#### Performance Concerns

Generic deriving can be very convenient for code generation, but it comes with a performance penalty. Consider defining a `Show` instance using `gShow` - instead of simply converting our data type directly to a `String`, we first convert it into a `GenericSpine`, and then convert that representation into a `String`. Creating this intermediate structure comes with a cost.

Now consider the `gEq` function, used to define `Eq` instances. We might be able to determine that two records are _not_ equal by comparing their first properties, for example. In this case, we only need to evaluate a small amount of the spine of each value, so the representation of `GenericSpine` uses laziness to avoid evaluating more of the spine than is necessary.

In the case of `foreign-generic`, the performance cost is often an acceptable trade-off for our increase in productivity, since we rarely need to parse or generate JSON in performance-critical sections of code in many applications.

