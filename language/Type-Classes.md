# Type Classes

PureScript supports type classes via the `class` and `instance` keywords.

Types appearing in class instances are must be of the form `String`, `Number`, `Boolean`, or `C t1 ... tn` where `C` is a type constructor (including `->` and `t_i` are types of the same form).

Type class instances are resolved based on the order in which they appeared in the source files. Overlapping instances are currently permitted but not recommended. In simple cases the compiler will display a warning and list the instances it found and which was chosen. In the future they may be disallowed completely.

Here is an example of the `Show` type class, with instances for `String`, `Boolean` and `Array`:

```purescript
class Show a where
  show :: a -> String

instance showString :: Show String where
  show s = s

instance showBoolean :: Show Boolean where
  show true = "true"
  show false = "false"

instance showArray :: (Show a) => Show (Array a) where
  show xs = "[" <> joinWith ", " (map show xs) <> "]"

example = show [true, false]
```

## Instance selection

Instance selection works by looking at the "head" types of each type class parameter. Functional dependencies also plays a role, but we'll not consider that for now to simplify the explanation. The "head" of a type is the top-most type constructor, for example the head of `Array Int` is `Array` and the head of `Record r` is `Record`.

``` purescript
class C t where
  f :: t -> String
-- Instances are chosen 
instance cRec :: C (Record r) where
  f _ = "this is a record"
instance cArr :: C (Array a) where
  f _ = "this is an array"

main = do
  -- The `f` function resolves to the instance for the argument type's head.
  log (f {}) -- Resolves to cRec's f.
  log (f []) -- Resolves to cArr's f.
-- Prints the following when executed.
this is a record
this is an array
```

## Multi-Parameter Type Classes

TODO. For now, see the section in [PureScript by Example](https://leanpub.com/purescript/read#leanpub-auto-multi-parameter-type-classes).

## Superclasses

Superclass implications can be indicated in a class declaration with a backwards fat arrow `<=`:

```purescript
class (Monad m) <= MonadFail m where
  fail :: forall a. String -> m a
```

This code example defines a `MonadFail` class with a `Monad` superclass: any type which defines an instance of `MonadFail` will be required to define an instance of `Monad` too.

Superclass instances will be used when searching for an instance of a subclass. For example, in the code below, the `Applicative` constraint introduced by the `pure` function can be discharged since `Applicative` is a superclass of `Monad`, which is in turn a superclass of `MonadFail`:

```purescript
assert :: forall m. (MonadFail m) => Boolean -> m Unit
assert true = pure unit
assert false = fail "Assertion failed"
```

## Orphan Instances

Type class instances which are defined outside of both the module which defined the class and the module which defined the type are called *orphan instances*. Some programming languages (including Haskell) allow orphan instances with a warning, but in PureScript, they are forbidden. Any attempt to define an orphan instance in PureScript will mean that your program does not pass type checking.

For example, the `Semigroup` type class is defined in the module `Data.Semigroup`, and the `Int` type is defined in the module `Prim`. If we attempt to define a `Semigroup Int` instance like this:

```purescript
module MyModule where

import Prelude

instance semigroupInt :: Semigroup Int where
  append = (+)
```

This will fail, because `semigroupInt` is an orphan instance. You can use a `newtype` to get around this:

```purescript
module MyModule where

import Prelude

newtype AddInt = AddInt Int

instance semigroupAddInt :: Semigroup AddInt where
  append (AddInt x) (AddInt y) = AddInt (x + y)
```

In fact, a type similar to this `AddInt` is provided in `Data.Monoid.Additive`, in the `monoid` package.

For multi-parameter type classes, the orphan instance check requires that the instance is either in the same module as the class, or the same module as at least one of the types occurring in the instance. (TODO: example)

## Kinds

Type class parameters do not need to be of kind Type. To explicitly specify a different kind, the parameter can be annotated with a kind signature. Unless the kind of a type parameter is explicitly annotated, it will be implicitly inferred based on how the type parameter is used in the type class's method signatures.

``` purescript
class A a
-- with a kind signature or methods, `a` defaults to kind `Type`.
class B (b :: Type)
-- `b` is specified as kind `Type`, so its instances must also be this kind.
class C c
  f :: forall a b. (a -> b) -> c a -> c b
-- `c`'s kind is unspecified, so it is inferred to be `Type -> Type` by its method `f`.
class D (d :: Type -> Type)
-- `D` has no methods, but we can still declare it to apply to types of kind `Type -> Type`.
class E (e :: # Type)
-- Row is its own kind, so we can specify a class to operate on types of rows, `# Type`.
```

The kinds of any types mentioned in an instance declaration must match the kinds of those parameters as determined by the type class's definition.

For example:

``` purescript
-- Functor can only be considered for types of kind `Type -> Type`.
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
-- Either is kind `Type -> Type -> Type`.
data Either a b = Left a | Right b
-- This means we can't write an instance of Functor for Either.
`instance functorEither :: Functor Either where ...` -- compile error
-- To make Either into kind `Type -> Type`, we simply include the first type parameter.
`instance functorEither :: Functor (Either e) where ...`
```

An effect of needing to adjust the kind of a data type to fit into a type class which applies to a lower arity kind, like with `Functor` and `Either` above, is that the behavior and laws of an instance will bias towards to the right-most type parameters.

## Rows

Concrete Row literals cannot appear in instances unless it's concrete type is fully determined by functional dependencies.

``` purescript
class M (t :: # Type)
-- Cannot write an instance for a specific row.
instance mClosedRow :: M ()
-- Can write an instance for a generic row.
instance mAnyRow :: M r

-- The row's type is determined by the type of `dt`.
class Ctors dt (ct :: # Type) | dt -> ct
-- Can use a row literal in instances of this class.
data X = X0 Int | X1 String
instance ctorsX :: Ctors X ("X0" :: Int, "X1" :: String)
data Y = Y0 Boolean
instance ctorsY :: Ctors Y ("Y0" :: Boolean)
```

A reason for rows being disallowed from appearing in instances is that PureScript doesn't allow orphan instances. Row has an unbounded/open set of labels and combination of labels and types. Each unique row has a unique type. A unique row value's type doesn't have a name in a module, so to avoid orphan instances, its instance of a class would need to be defined in the same module as the type class.

## Functional Dependencies

TODO. For now, see the section in [PureScript by Example](https://leanpub.com/purescript/read#leanpub-auto-functional-dependencies).

## Type Class Deriving

Some type class instances can be derived automatically by the PureScript compiler. To derive a type class instance, use the `derive instance` keywords:

```purescript
newtype Person = Person { name :: String, age :: Int }

derive instance eqPerson :: Eq Person
derive instance ordPerson :: Ord Person
```
Currently, the following type classes can be derived:

- [Data.Generic (class Generic)](https://pursuit.purescript.org/packages/purescript-generics/3.3.0/docs/Data.Generic#t:Generic)
- [Data.Generic.Rep (class Generic)](https://pursuit.purescript.org/packages/purescript-generics-rep/4.1.0/docs/Data.Generic.Rep#t:Generic)
- [Data.Eq (class Eq)](https://pursuit.purescript.org/packages/purescript-prelude/2.4.0/docs/Data.Eq#t:Eq)
- [Data.Ord (class Ord)](https://pursuit.purescript.org/packages/purescript-prelude/2.4.0/docs/Data.Ord#t:Ord)
- [Data.Functor (class Functor)](https://pursuit.purescript.org/packages/purescript-prelude/2.4.0/docs/Data.Functor#t:Functor)
- [Data.Newtype (class Newtype)](https://pursuit.purescript.org/packages/purescript-newtype/1.3.0/docs/Data.Newtype#t:Newtype)

## Compiler-Solvable Type Classes

Some type classes can be automatically solved by the PureScript Compiler without requiring you place a PureScript statement, like `derive instance`, in your source code.

``` purescript
foo :: forall t. (Warn "Custom warning message") => t -> t
foo x = x
```

Currently, the following type classes can be automatically solved:

- `Warn`
- [`IsSymbol`](https://pursuit.purescript.org/packages/purescript-typelevel-prelude/1.0.0/docs/Type.Data.Symbol#t:IsSymbol)
- [`CompareSymbol`](https://pursuit.purescript.org/packages/purescript-typelevel-prelude/1.0.0/docs/Type.Data.Symbol#t:CompareSymbol)
- [`AppendSymbol`](https://pursuit.purescript.org/packages/purescript-typelevel-prelude/1.0.0/docs/Type.Data.Symbol#t:AppendSymbol)
- `Union`, which computes the union of two rows of types
