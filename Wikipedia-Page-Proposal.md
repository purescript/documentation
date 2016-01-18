PureScript is a purely functional programming language which compiles to Javascript.

PureScript uses strict evaluation, and provides an expressive type system featuring a kind system, type classes, row polymorphism and rank-N types.

The first version of PureScript was released in September 2013 by Phil Freeman. As of January 2016, the project has over 60 contributors on GitHub. PureScript took part in the 2015 Google Summer of Code. 

[PureScript Conf 2015](https://github.com/purescript/purescript/wiki/PureScript-Conf-2015) was the first conference devoted to the PureScript language, with the [2016 edition](https://github.com/purescript/purescript/wiki/PureScript-Conf-2016) scheduled for May 2016. A [book](https://leanpub.com/purescript) on the language was released in 2015.

## Examples

### Hello World

```purescript
module Main where

import Control.Monad.Eff.Console (log)

main = log "Hello, World!"
```

## Features

### Type System

#### Algebraic Data Types

PureScript features _algebraic data types_, using a syntax similar to that of Haskell:

```purescript
data List a = Nil | Cons a (List a)
```

#### Type Inference

PureScript types are _inferred_ by the compiler, based on the types of subexpressions. For example:

```purescript
map f Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)
```

Here, the type of `f` does not need to be specified. PureScript will infer the correct type `forall a b. (a -> b) -> List a -> List b`.

The PureScript compiler provides warnings when type signatures are not provided on top-level declarations, encouraging users to provide types as a form of documentation.

#### Type Classes

PureScript supports type classes using a dictionary-passing transformation. 

For example, a type class for types with decidable equality is defined in the Prelude library:

```purescript
class Eq a where
  eq :: a -> a -> Bool
```

PureScript functions which are constrained by the `Eq` class are compiled to JavaScript functions, with an additional function argument which carries the `Eq` implementation as an object.

Type class instances are named in PureScript, using a double colon to separate an instance name from the instance head, as in a type declaration:

```purescript
instance eqList :: Eq a => Eq (List a) where
  eq Nil         Nil         = true
  eq (Cons x xs) (Cons y ys) = x == y && xs == ys
  eq _           _           = false
```

#### Extensible Records

PureScript represents records using Javascript objects at runtime, and implements _extensible records_ using row polymorphism.

For example, it is possible to write a PureScript function which extracts a `name` value from any record with a `name` property:

```purescript
getName rec = rec.name
```

PureScript will infer the following type for `getName`:

```purescript
getName :: forall r a. { name :: a | r } -> a
getName rec = rec.name
```

The type signature indicates that not only the type of the `name` field, but also the _remaining fields_ in the record, are left polymorphic.

Row polymorphism is also used to implement features such as record updates.

#### Extensible Effects

PureScript makes use of row polymorphism to implement its effect system.

### Generic Programming

### Foreign Function Interface

### Code Generation

### Tools

## History

## [Notable Sources](https://en.wikipedia.org/wiki/Wikipedia:Notability#General_notability_guideline)

### First-Party

- [Homepage](http://www.purescript.org)
- [Compiler repository](https://github.com/purescript/purescript)
- [Pursuit package search](http://pursuit.purescript.org)
- [eBook](https://leanpub.com/purescript)
- [Community Wiki](https://github.com/purescript/purescript/wiki)
- [Google Group](https://groups.google.com/group/purescript)

Ascending by date:

### Academic

| Date | Individual | Group | Link |
|--:|---|---|---|
| Apr 28, 2015 | Harry Garrood | Google Summer of Code | [Pursuit enhancements](https://www.google-melange.com/gsoc/project/details/google/gsoc2015/hdgarrood/5717271485874176) |
| Apr 28, 2015 | Nicolas Del Piano | Google Summer of Code | [Exhaustiveness Checker for PureScript](https://www.google-melange.com/gsoc/project/details/google/gsoc2015/ndelpiano/5685265389584384) |
| Jun 12, 2015 | Michal Srb | Imperial College London | [Haskell-Like S-Expression-Based Language Designed for an IDE](http://www.doc.ic.ac.uk/teaching/distinguished-projects/2015/m.srb.pdf) |

### News

| Date | Individual | Group | Link |
|--:|---|---|---|
| Sep 22, 2014 | Sergio De Simone | InfoQ | [PureScript: A Haskell-like Language that Compiles to JavaScript](http://www.infoq.com/news/2014/09/purescript-haskell-javascript) |
| Sep 15, 2015 | Steven Proctor | Functional Geekery | [Episode 26](https://www.functionalgeekery.com/episode-26-phil-freeman/) |
| Dec 9, 2015 | Joe Eames | JavaScript Jabber | [PureScript with John A. De Goes and Phil Freeman](https://devchat.tv/js-jabber/189-jsj-purescript-with-john-a-de-goes-and-phil-freeman) |

### Conference Videos

| Date | Individual | Group | Link |
|--:|---|---|---|
| Sep 21, 2014 | Bodil Stokke | Strange Loop | [PureScript (Maybe This Time We Get JavaScript Right)](https://youtu.be/yIlDBPiMb0o) |
| Jul 9, 2015 | Bodil Stokke | PolyConf | [Post FRP Frontend Programming](https://youtu.be/X5YBsy6PaDw) |
| Jul 20, 2015 | Boros Márton | Open Academy | [PureScript: the solution to the problem Javascript](https://youtu.be/WjlYD7HreAE) |
| Oct 8, 2015 | Vagmi Mudumbai | FnConf | [Pure functional programming in your browser & server with PureScript](https://youtu.be/fb44xcwHx0U) |
| Oct 17, 2015 | Michael Ficarra | Lambda Jam | [Getting Started With Purescript](https://youtu.be/OdenKPwSPss) |
| Dec 30, 2015 | John De Goes | LambdaConf | [Learn Functional Programming with PureScript](https://youtu.be/LqYfdmb0eUU) |
| Dec 30, 2015 | Phil Freeman | LambdaConf | [Purely Functional Web Apps using React and PureScript](https://youtu.be/qTYsxou0RE8) |