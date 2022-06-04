## Evaluation strategy

Unlike Haskell, PureScript is strictly evaluated.

As the evaluation strategy matches JavaScript, interoperability with existing code is trivial - a function exported from a PureScript module behaves exactly like any "normal" JavaScript function, and correspondingly, calling JavaScript via the FFI is simple too.

Keeping strict evaluation also means there is no need for a runtime system or overly complicated JavaScript output. It should also be possible to write higher performance code when needed, as introducing laziness on top of JavaScript comes with an unavoidable overhead.

## Prelude / base

There is no implicit `Prelude` import in PureScript, the `Prelude` module is just like any other. Also, no libraries are distributed with the compiler at all.

The generally accepted "standard" `Prelude` is the [`purescript-prelude`](https://github.com/purescript/purescript-prelude) library.

## Module Imports / Exports

Type classes in modules must be specifically imported using the `class` keyword.

```purescript
module B where

import A (class Fab)
```

There is no `qualified` keyword in PureScript. Writing `import Data.List as List` has the same effect as writing `import qualified Data.List as List` in Haskell.

Module imports and exports are fully documented on the [Modules](Modules.md) page.

## Types

### Explicit forall

Polymorphic functions in PureScript require an explicit `forall` to declare type variables before using them. For example, Haskell's list `length` function is declared like this:

``` haskell
length :: [a] -> Int
```

In PureScript this will fail with the error `Type variable a is undefined`. The PureScript equivalent is:

``` purescript
length :: forall a. Array a -> Int
```

A `forall` can declare multiple type variables at once, and should appear before typeclass constraints:

``` purescript
ap :: forall m a b. (Monad m) => m (a -> b) -> m a -> m b
```

### Numbers

There is a native `Number` type which represents JavaScript's standard IEEE 754 float and an `Int` which is restricted to the range of 32-bit integers. In JavaScript, the `Int` values and operations are generated with a `|0` postfix to achieve this, e.g. if you have variables `x`, `y`, and `z` of type `Int`, then the PureScript expression `(x + y) * z` would compile to `((x + y)|0 * z)|0`.

#### Strings

There is a native `String` type which is distinct from `Array Char`. A `String` consists of UTF-16 code units and may contain unpaired surrogates. Working with Unicode code points is supported by library functions. The set of supported escape sequences for string literals is different from both Haskell and JavaScript. 

#### Chars

PureScript has a type `Char` which represents a UTF-16 code unit for compatibility with JavaScript. In contrast, the type `Char` in Haskell represents a Unicode code point.

### Unit

PureScript has a type `Unit` used in place of Haskell's `()`. The `Prelude` module provides a value `unit` that inhabits this type.

### `[a]`

PureScript does not provide syntactic sugar for list types. Construct list types using `List` from `Data.List`.

There is also an `Array` type for native JavaScript arrays, but this does not have the same performance characteristics as `List`. `Array` _values_ can be constructed with `[x, y, z]` literals, but the type still needs to be annotated as `Array a`.

## Records

PureScript can encode JavaScript-style objects directly by using row types, so Haskell-style record definitions actually have quite a different meaning in PureScript:

``` purescript
data Point = Point { x :: Number, y :: Number }
```

In Haskell a definition like this would introduce several things to the current environment:

``` haskell
Point :: Number -> Number -> Point
x :: Point -> Number
y :: Point -> Number
```

However in PureScript this only introduces a `Point` constructor that accepts an object type. In fact, often we might not need a data constructor at all when using object types:

``` purescript
type PointRec = { x :: Number, y :: Number }
```

Objects are constructed with syntax similar to that of JavaScript (and the type definition):

``` purescript
origin :: PointRec 
origin = { x: 0, y: 0 }
```

And instead of introducing `x` and `y` accessor functions, `x` and `y` can be read like JavaScript properties:

``` purescript
originX :: Number
originX  = origin.x
```

PureScript also provides a record update syntax similar to Haskell's:

``` purescript
setX :: Number -> PointRec -> PointRec 
setX val point = point { x = val }
```

A common mistake to look out for is when writing a function that accepts a data type like the original `Point` above—the object is still wrapped inside `Point`, so something like this will fail:

``` purescript
showPoint :: Point -> String
showPoint p = show p.x <> ", " <> show p.y
```

Instead, we need to destructure `Point` to get at the object:

``` purescript
showPoint :: Point -> String
showPoint (Point obj) = show obj.x <> ", " <> show obj.y
```

## Type classes

### Arrow direction

When declaring a type class with a superclass, the arrow is the other way around. For example:

```purescript
class (Eq a) <= Ord a where
  ...
```

This is so that `=>` can always be read as logical implication; in the above case, an `Ord a` instance _implies_ an `Eq a` instance.

### Named instances

In PureScript, you can give names to instances:

```purescript
instance arbitraryUnit :: Arbitrary Unit where
  ...
```

Although instance names are optional, they can help the readability of compiled JavaScript. Overlapping instances are still disallowed, like in Haskell.

### Deriving

Unlike Haskell, PureScript doesn't have deriving functionality when declaring
data types.  For example, the following code does not work in PureScript:

```haskell
data Foo = Foo Int String deriving (Eq, Ord)
```

However, PureScript does have `StandaloneDeriving`-type functionality:

```purescript
data Foo = Foo Int String

derive instance eqFoo :: Eq Foo
derive instance ordFoo :: Ord Foo
```

Examples of type classes that can be derived this way include `Eq`, `Functor`,
and `Ord`.  See
[here](https://github.com/purescript/documentation/blob/master/language/Type-Classes.md#type-class-deriving)
for a list of other type classes.

Furthermore, using generics, it is also possible to use generic implementations for type classes like `Bounded`, `Monoid`, and `Show`. See the [deriving guide](../guides/Type-Class-Deriving.md#deriving-from-generic) for more information.
### Orphan Instances

Unlike Haskell, orphan instances are completely disallowed in PureScript.  It is a compiler error to try to declare orphan instances.

When instances cannot be declared in the same module, one way to work around it is to use [newtype wrappers](http://stackoverflow.com/questions/22080564/whats-the-practical-value-of-all-those-newtype-wrappers-in-data-monoid).

### Default members

At the moment, it is not possible to declare default member implementations for type classes. This may change in the future.

### Type class hierarchies

Many type class hierarchies are more granular than in Haskell. For example:

* `Category` has a superclass `Semigroupoid` which provides `(<<<)`, and does not require an identity.
* `Applicative` has a superclass `Apply`, which provides `(<*>)` and does not require an implementation for `pure`.

## Overloaded number literals

In PureScript, number literals are not overloaded as in Haskell. That is, `1` is always an `Int`, and `1.0` is always a `Number`.

## Tuples

PureScript has no special syntax for tuples as records can fulfill the same role that *n*-tuples do with the advantage of having more meaningful types and accessors.

A `Tuple` type for 2-tuples is available via the [purescript-tuples](https://github.com/purescript/purescript-tuples) library. `Tuple` is treated the same as any other type or data constructor. There is a shorthand `a /\ b` for `Tuple a b` in [Data.Tuple.Nested](https://pursuit.purescript.org/packages/purescript-tuples/6.0.1/docs/Data.Tuple.Nested).

## Composition operator

PureScript uses `<<<` rather than `.` for right-to-left composition of functions. This is to avoid a syntactic ambiguity with `.` being used for property access and name qualification. There is also a corresponding `>>>` operator for left-to-right composition.

The `<<<` operator is actually a more general morphism composition operator that applies to semigroupoids and categories, and the `Prelude` module provides a `Semigroupoid` instance for the `->` type, which gives us function composition.

## `return`

In the past, PureScript used `return`. However, it is now removed and replaced with [`pure`](https://pursuit.purescript.org/packages/purescript-prelude/docs/Control.Applicative#v:pure). It was always an alias for `pure`, which means this change was implemented by simply removing the alias.

## Array Comprehensions

PureScript does not provide special syntax for array comprehensions. Instead, use `do`-notation. The `guard` function from the `Control.Alternative` module in `purescript-control` can be used to filter results:

```purescript
import Prelude (($), (*), (==), bind, pure)
import Data.Array ((..))
import Data.Tuple (Tuple(..))
import Control.Alternative (guard)

factors :: Int -> Array (Tuple Int Int)
factors n = do
  a <- 1 .. n
  b <- 1 .. a
  guard $ a * b == n
  pure $ Tuple a b
```

## No special treatment of `$`

GHC provides a special typing rule for the `$` operator, so that the following natural application to the rank-2 `runST` function is well-typed:

```haskell
runST $ do
  ...
```

PureScript does not provide this rule, so it is necessary to either 

- omit the operator: `runST do ...`
- or use parentheses instead: `runST (do ...)`

## Defining Operators

In Haskell, it is possible to define an operator with the following natural syntax:

```haskell
f $ x = f x
```

In PureScript, you provide an operator alias for a named function. Defining functions using operators is removed since version 0.9.

```purescript
apply f x = f x
infixr 0 apply as $
```

## Operator Sections

In Haskell, there is syntactic sugar to partially apply infix operators.

```haskell
(2 ^) -- desugars to `(^) 2`, or `\x -> 2 ^ x`
(^ 2) -- desugars to `flip (^) 2`, or `\x -> x ^ 2`
```

In PureScript, operator sections look a little bit different.

```purescript
(2 ^ _)
(_ ^ 2)
```

## Fixity declarations for functions

In Haskell, functions can have fixity declarations. For example in `base`:

```haskell
infix  4 `elem`, `notElem`
infixl 7 `quot`, `rem`, `div`, `mod`
```

However fixity declarations are only supported while declaring [operator aliases](Syntax.md#binary-operators) for functions in PureScript. Functions are always left associative and have the highest precedence when used as infix operators, which means that:

```
x * y `mod` z
```

parses as:

```haskell
((x * y) `mod` z)
```

in Haskell, but as:

```purescript
(x * (y `mod` z))
```

in PureScript, and those expressions may yield different values:

```purescript
> ((2 * 3) `mod` 5)
1
```

```purescript
> (2 * (3 `mod` 5))
6
```

## Extensions

The PureScript compiler does not support GHC-like language extensions. However, there are some "built-in" language features that are equivalent (or at least similar) to a number of GHC extensions. These currently are:

* [ApplicativeDo](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/applicative_do.html#extension-ApplicativeDo)
* [BlockArguments](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/block_arguments.html#extension-BlockArguments)
* [DataKinds](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/data_kinds.html#extension-DataKinds) (see note below)
* [DeriveFunctor](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/deriving_extra.html#extension-DeriveFunctor)
* [DeriveGeneric](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/generics.html#extension-DeriveGeneric)
* [EmptyDataDecls](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/nullary_types.html#extension-EmptyDataDecls)
* [ExplicitForAll](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/explicit_forall.html#extension-ExplicitForAll)
* [FlexibleContexts](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/flexible_contexts.html#extension-FlexibleContexts)
* [FlexibleInstances](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/instances.html#extension-FlexibleInstances)
* [FunctionalDependencies](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/functional_dependencies.html#extension-FunctionalDependencies)
* [GeneralisedNewtypeDeriving](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/newtype_deriving.html#extension-GeneralisedNewtypeDeriving)
* [InstanceSigs](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/instances.html#extension-InstanceSigs)
* [KindSignatures](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/kind_signatures.html#extension-KindSignatures)
* [LambdaCase](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/lambda_case.html#extension-LambdaCase)
* [LiberalTypeSynonyms](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/liberal_type_synonyms.html#extension-LiberalTypeSynonyms)
* [MonoLocalBinds](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/let_generalisation.html#extension-MonoLocalBinds)
* [MultiParamTypeClasses](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/multi_param_type_classes.html#extension-MultiParamTypeClasses)
* [NamedFieldPuns](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/record_puns.html#extension-NamedFieldPuns)
* [NoImplicitPrelude](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/rebindable_syntax.html#extension-NoImplicitPrelude)
* [NumericUnderscores](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/numeric_underscores.html#extension-NumericUnderscores)
* [PartialTypeSignatures](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/partial_type_signatures.html#extension-PartialTypeSignatures)
* [PolyKinds](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/poly_kinds.html#extension-PolyKinds)
* [RankNTypes](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/rank_polymorphism.html#extension-RankNTypes)
* [RebindableSyntax](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/rebindable_syntax.html#extension-RebindableSyntax)
* [RoleAnnotations](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/roles.html#extension-RoleAnnotations)
* [ScopedTypeVariables](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/scoped_type_variables.html#extension-ScopedTypeVariables)
* [StandaloneDeriving](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/standalone_deriving.html#extension-StandaloneDeriving)
* [StandaloneKindSignatures](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/standalone_kind_signatures.html#extension-StandaloneKindSignatures)
* [TypeOperators](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/type_operators.html#extension-TypeOperators)
* [TypeSynonymInstances](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/instances.html#extension-TypeSynonymInstances)
* [UndecidableInstances](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/instances.html#extension-UndecidableInstances)
* [UndecidableSuperClasses](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/undecidable_super_classes.html#extension-UndecidableSuperClasses)
* [UnicodeSyntax](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/unicode_syntax.html#extension-UnicodeSyntax)

Note on `DataKinds`: Unlike in Haskell, user-defined kinds are open, and they are not promoted, which means that their constructors can only be used in types, and not in values. For more information about the kind system, see https://github.com/purescript/documentation/blob/master/language/Types.md#kind-system

## `error` and `undefined`

For `error`, you can use `Effect.Exception.Unsafe.unsafeThrow`, in the `purescript-exceptions` package.

`undefined` can be emulated with `Unsafe.Coerce.unsafeCoerce unit :: forall a. a`, which is in the `purescript-unsafe-coerce` package. See also https://github.com/purescript/purescript-prelude/issues/44.

Although note that these might have different behaviour to the Haskell versions due to PureScript's strictness.

## Documentation comments

When writing documentation, the pipe character `|` must appear at the start of every comment line, not just the first. See [the documentation for doc-comments](Syntax.md#comments) for more details.

## Where is ... from Haskell?

As PureScript has not inherited Haskell's legacy code, some operators and functions that are common in Haskell have different names in PureScript:

- `(>>)` is `(*>)`, as `Apply` is a superclass of `Monad` so there is no need to have an `Monad`-specialised version.
- Since 0.9.1, the `Prelude` library does not contain `(++)` as a second alias for `append` / `(<>)` (`mappend` in Haskell) anymore.
- Haskell's `<&>` operator (from `Data.Functor`) is equivalent to Purescript's `<#>` (operator alias to `mapFlipped`).
- `mapM` is `traverse`, as this is a more general form that applies to any traversable structure, not just lists. Also it only requires `Applicative` rather than `Monad`. Similarly, `liftM` is `map`.
- Many functions that are part of `Data.List` in Haskell are provided in a more generic form in `Data.Foldable` or `Data.Traversable`.
- `some` and `many` are defined with the type of list they operate on (`Data.Array` or `Data.List`).
- Instead of `_foo` for typed holes, use `?foo`. You have to name the hole; `?` is not allowed.
- Ranges are written as `1..2` rather than `[1..2]`. There's a difference, though: in Haskell `[2..1]` is an empty list, whereas in PureScript `2..1` is `[2,1]`
