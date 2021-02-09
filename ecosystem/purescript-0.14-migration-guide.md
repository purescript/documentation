# PureScript 0.14 Migration Guide

This guide summarizes the changes you may need to make to your code to migrate from PureScript 0.13 to PureScript 0.14. It covers major changes to the compiler, the core libraries, and tooling.

Related resources:
- The PureScript 0.14 [release announcement]()
- The PureScript 0.14 [compiler release notes]()

Compiler releases are often accompanied by breaking changes in the core libraries. While some major library changes are described in this document, you should consult the individual changelogs for any libraries you depend on.

**Contents**

1. [Compiler Changes](#compiler-changes)
1. [Library Changes](#library-changes)
1. [Tooling Changes](#tooling-changes)

## Compiler Changes

The PureScript 0.14 compiler introduces a number of new features, including support for polymorphic kinds and zero-cost coercions. These features may require changes to your code.

This section of the guide will walk through common changes you may need to make, but it isn't a complete overview of changes in the 0.14 compiler and doesn't provide an explanation of these new features. You should also consult the [PureScript 0.14 compiler release notes]().

### Polykinds & Type In Type

PureScript now supports polymorphic kinds and kind signatures. Most code is unaffected, but there are some changes.

The type checker now supports `TypeInType`, so the old `Kind` data type and namespace has been removed. The `foreign import kind` syntax is deprecated, so this will warn:

```purs
foreign import kind Boolean
foreign import data True :: Boolean
foreign import data False :: Boolean
```

and should be rewritten as an empty data declaration:

```purs
data Boolean
foreign import data True :: Boolean
foreign import data False :: Boolean
```

The old unary `# Type` syntax for row kinds is deprecated, and it should instead be written as `Row Type`.

```diff
- class ListToRow (list :: RowList) (row :: # Type)
+ class ListToRow (list :: RowList) (row :: Row Type)
```

Finally, all type-level declarations are now generalized.

```purs
data Proxy a
```

This previously had the `Type`-defaulted kind `Type -> Type`. Now this will be generalized to `forall k. k -> Type`. It is better to be explicit about polymorphism by writing signatures, and the compiler will warn about missing kind signatures when it infers polymorphic kinds.

To avoid the compiler warning, provide a kind signature:

```purs
data Proxy :: forall k. k -> Type
data Proxy a = Proxy
```

Classes can have signatures too, and they must end with a `Constraint` kind instead of `Type`. For example, here's the new definition of `Prim.Row.Cons`:

```purs
class Cons :: forall k. Symbol -> k -> Row k -> Row k -> Constraint
class Cons label a tail row | label a tail -> row, label row -> a tail
```

### Coercible & Role Annotations

PureScript 0.14 introduces a new compiler-solved class, `Prim.Coerce.Coercible`, to relate types with the same runtime representation. You can now use `Safe.Coerce.coerce` from the new `safe-coerce` library in place of `Unsafe.Coerce.unsafeCoerce` to turn an `a` into a `b` when `Coercible a b` holds.

If you are currently using `unsafeCoerce` to transform two types with the same runtime representation, you should consider replacing it with `coerce`.

The `Coercible` implementation introduces role annotations for type parameters (to learn more about roles, see the compiler release notes). Role annotations can loosen or strengthen the compiler-inferred roles of type parameters.

For example, the compiler infers nominal roles for foreign data types, which is safe but can be too constraining. For example, we should be able to coerce `Effect Age` to `Effect Int` if `Age` is a newtype over `Int`, but because `Effect` is a foreign data type its type parameter is inferred with a nominal role. We can loosen the role with an annotation:

```purs
foreign import data Effect :: Type -> Type

-- this is a role annotation, setting Effect's type parameter to be
-- representational instead of nominal (its inferred role).
type role Effect representational
```

In contrast, it is unsafe to allow coercions between `Map k1 a` and `Map k2 a`, even when `Coercible k1 k2` holds. We can annotate `Map`'s key type parameter with a nominal role to prevent coercions:

```purs
data Map k v = ...

-- this is a role annotation, setting `Map`'s key type parameter to
-- be nominal instead of representational (its inferred role).
type role Map nominal representational
```

### Other Changes

Primes are no longer allowed in JavaScript FFI definitions because PureScript 0.15 will generate ES modules.

**To fix:**

Rename any functions that contain a prime and are implemented via the FFI:

```js
// before
exports["functionName'"] = function (a) { return a; }

// after: these are common options
exports.functionNameImpl = function (a) { return a; }
exports._functionName = function (a) { return a; }
exports.functionNamePrime = function (a) { return a; }
```

## Library Changes

Several core library changes accompanied the PureScript 0.14 compiler release. This guide covers notable changes to the `purescript` (core), `purescript-contrib`, `purescript-node`, and `purescript-web` organizations.

However, the changes listed below are not exhaustive. As of the PureScript 0.14 release, all libraries in these organizations contain a CHANGELOG.md file which lists any breaking changes in their latest release. You should consult the changelogs for any libraries you depend on in addition to this migration guide.

### Library Restructuring

Several core libraries have been restructured. In almost all cases functionality has been preserved, but some functions, types, and / or instances have been moved to new locations.

#### Monomorphic proxies and the `proxy` library have been deprecated, with `Proxy` merged into `prelude`.

PureScript 0.14 introduced support for polymorphic kinds, which means we no longer need a proliferation of different proxy types which differ only in the kind of their parameter. For example, we previously had many proxy types:

```purs
data Proxy (a :: Type) = Proxy
data Proxy2 (a :: Type -> Type) = Proxy2
data Proxy3 (a :: Type -> Type -> Type) = Proxy3
data SProxy (a :: Symbol) = SProxy
data RProxy (row :: # Type) = RProxy
data RLProxy (row :: RowList) = RLProxy
-- ...and so on.
```

All proxy types except for `Proxy` have been deprecated, and `Proxy` is now implemented with a polymorphic kind:

```purs
-- | Proxy type for all kinds.
data Proxy :: forall k. k -> Type
data Proxy a = Proxy
```

In addition, the `proxy` library has been deprecated and all relevant functionality moved into `prelude`.

**To fix:**

You should remove `proxy` as a dependency of your library or application, as the relevant code now lives in the `prelude` package.

You do not need to update code using proxy types like `Proxy2` and `SProxy`, because these types have been deprecated but will not be removed until the PureScript 0.15 compiler release. However, if you'd like to get ahead of this change, you should replace all uses of proxy types other than `Proxy` to use `Proxy` instead. For example:

```purs
-- Code written pre-PureScript 0.14, which will compile with PureScript 0.14
-- but will not compile with PureScript 0.15.
getFoo = Record.get (SProxy :: SProxy "foo")

-- Code written for PureScript 0.14 which will continue to work with
-- PureScript 0.15.
getFoo = Record.get (Proxy :: Proxy "foo")
```


#### The `generics-rep` library has been merged into `prelude`

`Generic` has been moved to the bottom of the core ecosystem because it's widely-used class with compiler support, but it didn't previously cover some common types like `Tuple` or `Either`. This change enables all of the data types across the core libraries which have a sensible `Generic` instance to have one.

**To fix:**

You should remove `generics-rep` as a dependency of your library or application, as most of the relevant code and instances live in the `prelude` package. Code related to `Enum` has been moved to the `enums` package, but you should already depend on this package if you were using it.

You should rename any modules that used `Generic` instances for common type classes, as those modules have been renamed.

```txt
Data.Generic.Rep.Bounded        -> Data.Bounded.Generic
Data.Generic.Rep.Enum           -> Data.Enum.Generic (in the `enums` package)
Data.Generic.Rep.Eq             -> Data.Eq.Generic
Data.Generic.Rep.HeytingAlgebra -> Data.HeytingAlgebra.Generic
Data.Generic.Rep.Monoid         -> Data.Monoid.Generic
Data.Generic.Rep.Ord            -> Data.Ord.Generic
Data.Generic.Rep.Ring           -> Data.Ring.Generic
Data.Generic.Rep.Semigroup      -> Data.Semigroup.Generic
Data.Generic.Rep.Semiring       -> Data.Semiring.Generic
Data.Generic.Rep.Show           -> Data.Show.Generic
```

The `argonaut-generic` library has also renamed its modules to match this naming convention:

```txt
Data.Argonaut.Decode.Generic.Rep -> Data.Argonaut.Decode.Generic
Data.Argonaut.Encode.Generic.Rep -> Data.Argonaut.Encode.Generic Data.Argonaut.Types.Generic.Rep  -> Data.Argonaut.Types.Generic
```

#### The `globals` library has been deprecated

The `globals` library provided various functions and values that are in the global scope in JavaScript. The library has been deprecated in particular because PureScript supports multiple backends besides JavaScript, and the relevant functions have been moved or reimplemented in other libraries.

Specifically:

* Number values and operations like `isFinite`, parsing, and formatting have been moved to the `numbers` package, which itself has moved into the core organization.
* Integer parsing has been removed, as it is already included in the `integers` package.
* Encoding and decoding URIs and URI components using JavaScript's builtin `encodeURIComponent` and `decodeURIComponent` have been moved to the new `js-uri` package.
* Unsafe global functions have been removed altogether.

**To fix:**

You should remove the `globals` dependency from your library or application.

If you used any `Number`-related code from the `globals` package, then you should add a new dependency on `numbers` and rename your imports:

```diff
- import Global (isNan, nan, isFinite, infinity)
+ import Data.Number (isNan, nan, isFinite, infinity)
 
- import Global (toFixed, toPrecision, toExponential)
+ import Data.Number.Format (toStringWith, fixed, precision, exponential)

  -- toFixed, toPrecision, and toExponential can be replaced as follows:
- x = toFixed someInt 4.0 
+ x = toStringWith (fixed someInt) 4.0
 
- y = toPrecision someInt 4.0
+ y = toStringWith (precision someInt) 4.0
 
- z = toExponential intArg 4.0
+ z = toStringWith (exponential someInt) 4.0
```

If you used `readInt`, then you should add a dependency on `integers` and use the `fromStringAs` function given the correct radix, or if you are parsing a base-10 integer then you can use `Data.Number.fromString`.

```diff
- import Global (readInt)
+ import Data.Int (radix, fromStringAs)

  x :: String -> Maybe Int
- x = readInt 2
+ x = fromStringAs (unsafeRadix 2)
+   where
+   unsafeRadix n = unsafePartial fromJust $ radix n
```

If you used URI encoding and decoding functions, then you should add a dependency on `js-uri` and update your imports:

```diff
- import Global (decodeURI, decodeURIComponent, encodeURI, encodeURIComponent)
+ import JSURI (decodeURI, decodeURIComponent, encodeURI, encodeURIComponent)
```

If you used various unsafe functions exposed by the `globals` package, you can in most cases use functions from the `partials` package (like `unsafePartial`) combined with partial functions like `Data.Maybe.fromJust` to produce an unsafe function from a safe one.

#### The dependency graph for `functors` and related packages have changed in order to unify newtypes among the libraries.

Implemented in [purescript-foldable-traversable#131](https://github.com/purescript/purescript-foldable-traversable/pull/131), [purescript-functors#31](https://github.com/purescript/purescript-functors/pull/31), [purescript-profunctor#41](https://github.com/purescript/purescript-profunctor/pull/41), [purescript-distributive#17](https://github.com/purescript/purescript-distributive/pull/17), [purescript-contravariant#30](https://github.com/purescript/purescript-contravariant/pull/30), [purescript-bifunctors#22](https://github.com/purescript/purescript-bifunctors/pull/22), [purescript-tuples#46](https://github.com/purescript/purescript-tuples/pull/46), [purecsript-identity#26](https://github.com/purescript/purescript-identity/pull/26), [purescript-either#64](https://github.com/purescript/purescript-either/pull/64), and [purecsript-const#19](https://github.com/purescript/purescript-const/pull/19). 

Mostly discussed in [purescript-profunctor#23](https://github.com/purescript/purescript-profunctor/issues/23).

A number of dependencies have been rearranged in the core libraries so that more types can be reused among them (especially newtypes between `profunctor` and `bifunctors`) and so that fundamental data types like `either` and `tuple` are at the bottom of the dependency hierarchy.

Most of these changes involve instances and should be invisible to user code. However, some functions and modules have changed locations and will require changes in your code if you use them.

**To fix:**

The `lookup` function has moved from `Data.Tuple` to `Data.Foldable` and the import should be updated accordingly:

```purs
- import Data.Tuple (lookup)
+ import Data.Foldable (lookup)
```

Several modules have moved or been renamed, all from the `profunctors` and `bifunctors` packages into the `functors` package. You'll need to update your imports accordingly:

```
Data.Profunctor.Clown  -> Data.Functor.Clown
Data.Profunctor.Cowrap -> Data.Functor.Flip
Data.Profunctor.Joker  -> Data.Functor.Joker
Data.Profunctor.Costar -> Data.Functor.Costar

Data.Bifunctor.Clown   -> Data.Functor.Clown
Data.Bifunctor.Flip    -> Data.Functor.Flip
Data.Bifunctor.Joker   -> Data.Functor.Joker
Data.Bifunctor.Product -> Data.Functor.Product2
```

The `Data.Profunctor.Wrap` and `Data.Bifunctor.Wrap` types have been deleted, as any profunctor or bifunctor should also have a functor instance.

This was the old dependency graph:

![old-dependency-graph](https://user-images.githubusercontent.com/1570964/106351480-6e55a100-62aa-11eb-8e8a-72cb33e354dc.png)

This is the new dependency graph, where the dotted line represents three packages which may be merged together in the future:

![new-dependency-graph](https://user-images.githubusercontent.com/1570964/106351479-6ac21a00-62aa-11eb-8234-803f7868cc53.png)

### Notable Library Changes

#### The `Newtype` class now has `Coercible` as a superclass and no longer has `wrap` and `unwrap` class members.

Implemented in [purescript-newtype#22](https://github.com/purescript/purescript-newtype/pull/22).

The `Newtype` class exists to support convenient wrapping and unwrapping and to implement the combinators in the `Data.Newtype` module. The `Newtype` class now has `Coercible` as a superclass, which allows most combinators to now be implemented with `coerce` from the new `safe-coerce` library. This makes these combinators zero-cost for a performance gain.

The `Newtype` class no longer has `wrap` and `unwrap` members. Instead, `wrap` and `unwrap` are synonyms for `coerce` and are exported as functions from the module. Most code does not need to change, but if you manually defined a `Newtype` instance in your code it will no longer compile.

In addition, newtypes that do not have their constructors exported can no longer be unwrapped via the `Newtype` class. Most code will be unaffected, as giving a `Newtype` instance to a newtype with hidden constructors was already considered bad practice and usually accidental.

**To fix:**

Most `Newtype` instances are derived and will continue to work. If you manually defined a `Newtype` instance for a type, however, you can simply remove the `wrap` and `unwrap` definitions:

```diff
+ instance newtypeAdditive :: Newtype (Additive a) a
- instance newtypeAdditive :: Newtype (Additive a) a where
-   wrap = Additive
-   unwrap (Additive a) = a
```

Most newtypes either export their constructors and have a `Newtype` instance or hide their constructors and do not have the instance. However, if you have a newtype in your code which has hidden constructors and a `Newtype` instance then you will need to either:

1. Remove the `Newtype` instance (a breaking change), or
1. Export the newtype constructors so that `wrap` and `unwrap` can be used via the `Coercible` class.

#### The `MonadZero` class has been deprecated.

Implemented in [purescript-control#64](https://github.com/purescript/purescript-control/pull/64) with additional discussion in [purescript-control#62](https://github.com/purescript/purescript-control/issues/62) and [purescript-control#51](https://github.com/purescript/purescript-control/issues/51).

The `MonadZero` class has been deprecated, as the additional law that it provides (`empty >>= f = empty`) is already covered by the classes it requires, namely `Monad` and `Alternative`.

The `MonadZero` class was already little-used; in the core libraries it only shows up as a superclass of `MonadPlus` and as a constraint on the `guard` function. This release deprecates the class and it will be removed in the next release.

Accordingly, the `guard` function has been moved from `Control.MonadZero` to `Control.Alternative` and the `MonadZero m` constraint has been replaced with `Monad m => Alternative m => ...`.

**To fix:**

If you are using the `guard` function, update your imports:

```diff
- import Control.MonadZero (guard)
+ import Control.Alternative (guard)
```

If you are using the `MonadZero m` constraint, replace it with `Monad m` and `Alternative m`:

```diff
- x :: forall m. MonadZero m => ...
+ x :: forall m. Monad m => Alternative m => ...
```

#### The `Foldable1` class added `foldl1` and `foldr1` as members.

Implemented in [purescript-foldable-traversable#121](https://github.com/purescript/purescript-foldable-traversable/pull/121), with additional discussion in [purescript-foldable-traversable#110](https://github.com/purescript/purescript-foldable-traversable/issues/110).

The `Foldable` class provides three ways to fold: from the left via `foldl`, from the right via `foldr`, or via `foldMap`. However, the `Foldable1` class only defined two ways to fold, without specifying direction: `fold1` and `foldMap1`. It was possible to implement `foldr1` or `foldl1` in terms of these functions, but this removed the ability for instances to provide a performant implementation for these folds.

The `Foldable1` class now has three type class members: `foldl1`, `foldr1`, and `foldMap1`, mirroring the `Foldable` class. The `fold1` function is no longer a type class member and is instead defined in the module via `foldMap1 identity`.

This is a breaking change because data types that implement `Foldable1` need to update their instances to remove `fold1` and add `foldl1` and `foldr1`. `Foldable1` provides default implementations that you can use to implement your instances.

**To fix:**

If you currently define an instance of `Foldable1`, then you should remove the `fold1` implementation and add new implementations for `foldl1` and `foldr1`. You can implement your instance by hand, or you can use one of the default implementations provided by the `Data.Semigroup.Foldable` module:

* `foldl1Default` for `foldl1`
* `foldr1Default` for `foldr1`
* `foldMap1DefaultL` or `foldMapDefaultR` for `foldMap1`.

Not all of these default implementations can safely be used together; any unsafe interactions are described in the documentation comments for each of these default functions.

Here's an example of updating a `Foldable1` instance:

```diff
  instance foldable1Coyoneda :: Foldable1 f => Foldable1 (Coyoneda f) where
    foldMap1 f = unCoyoneda \k -> foldMap1 (f <<< k)
-   fold1 = unCoyoneda \k -> foldMap1 k
+   foldr1 = foldr1Default
+   foldl1 = foldl1Default
```

#### The `Semigroup` and `Monoid` instances for `Map` have been removed.

Implemented in [purescript-ordered-collections#38](https://github.com/purescript/purescript-ordered-collections/pull/38), with additional discussion in [purescript-ordered-collections#36](https://github.com/purescript/purescript-ordered-collections/issues/36) and the [Unbiasing the Semigroup instance for Map](https://discourse.purescript.org/t/unbiasing-the-semigroup-instance-for-map/1935) proposal on Discourse.

The `Semigroup` instance for `Map a` was previously left-biased, which meant that it would preserve the first value and throw away the second value when appending. This instance has been removed for several reasons

1. The left-biased instance is less useful than an unbiased instance.
1. The `Semigroup` instance for other types like `Object a` and `Maybe a` is unbiased, which means that when appending values the instance defers to the underlying `Semigroup` (ie. `Semigroup a => Object a`).
1. The left-biased instance can be recovered using the [First](https://pursuit.purescript.org/packages/purescript-prelude/4.1.1/docs/Data.Semigroup.First#t:First) newtype, and a right-biased instance can be recovered using the [Last](https://pursuit.purescript.org/packages/purescript-prelude/4.1.1/docs/Data.Semigroup.Last) newtype.

The `Semigroup` instance for `Map` will be restored in PureScript 0.15, but will be unbiased rather than left-biased. The instance is removed in PureScript 0.14 to avoid silently changing the behavior of existing code. Since the `Monoid` class requires `Semigroup`, the `Monoid` instance for `Map` has also been removed.

To mitigate breaking changes, a new `SemigroupMap` newtype has been added to the `Data.Map` module which supplies an unbiased `Semigroup` instance.

**To fix:**

If you previously relied on the `Monoid` instance for `Data.Map` (ie. you used `mempty`), then you may be able to simply update your code to use `Map.empty` directly instead:

```diff
  import Data.Map as Map

  m :: Map Int String
- m = mempty
+ m = Map.empty
```

If this is not possible and you previously relied on the `Semigroup` or `Monoid` instances for `Data.Map`, then you can use the `SemigroupMap` newtype to continue using those instances. Note that the behavior of the instances has changed to be unbiased rather than left-biased. You can use the `First` and `Last` newtypes to produce left-biased or right-biased behavior, as demonstrated below.

```purs
import Data.Map (SemigroupMap, singleton)
import Data.Semigroup.First (First(..))
import Data.Semigroup.Last (Last(..))

-- Pseudocode demonstrating evaluation
let
  s :: forall key value. key -> value -> SemigroupMap key value
  s k v = SemigroupMap (Data.Map.singleton k v)

(s 1     "foo") <> (s 1     "bar") == (s 1  "foobar")
(s 1 (First 1)) <> (s 1 (First 2)) == (s 1 (First 1))
(s 1  (Last 1)) <> (s 1  (Last 2)) == (s 1  (Last 2))
```

#### The `Semigroup` instance for foreign objects has changed.

Implemented in [purescript-foreign-object#23](https://github.com/purescript/purescript-foreign-object/pull/23) and [purescript-foreign-object#19](https://github.com/purescript/purescript-foreign-object/pull/19).

The `Semigroup` instance for `Foreign.Object.Object` previously appended values from the right to values from the left, which is in reverse order from how this instance should have been implemented. To demonstrate:

```purs
-- these were previously equivalent
singleton k a <> singleton k b == singleton k (b <> a)

-- now these are equivalent, as expected
singleton k a <> singleton k b == singleton k (a <> b)
```

Code using these instances will now append in the correct order.

**To fix:**

Most users should not change their code and will now be using the correct instance. However, if you were intentionally relying on the behavior of this instance then you can recover it by writing a newtype around `Object` and implementing this newtype instance as desired. For example:

```purs
newtype Reversed v = Reversed (Object v)

instance semigroupReversed :: Semigroup v => Semigroup (Reversed v) where
  append (Reversed l) (Reversed r) = Reversed (r <> l)
```

#### The `Alt` instance for `ZipList` has changed.

Implemented in [purescript-lists#150](https://github.com/purescript/purescript-lists/pull/150).

The previous `Alt` instance for `ZipList` relied on a simple append, which doesn't satisfy the `Alternative` distributivity law:

```purs
(f <|> g) <*> x == (f <*> x) <|> (g <*> x)
```

Using an instance implemented with `append`, the left-hand side will be a list of length `min(length f + length g, x)`, whereas the right-hand side will be a list of length `min(length f, length x) + min(length g, length x)`. For example, given:

```purs
f = g = ZipList ((_ + 1) : Nil)
x = ZipList (0 : Nil)
```

we get the left-hand side as `ZipList (1 : Nil)` but the right-hand side `ZipList (1 : 1 : Nil)`.

**To fix:**

Most users will not need to change their code, as this is a bug fix for incorrect behavior. However, if you were previously relying on the specific behavior of this instance, then you can create your own newtype around `List` which defines an `Alt` instance with `append`:

```purs
newtype MyZipList = MyZipList List

instance altMyZipList :: Alt MyZipList where
  alt = append
```

#### The `Record.Builder.merge` and `Record.Builder.union` functions have changed.

Implemented in [purescript-record#73](https://github.com/purescript/purescript-record/pull/73) with additional discussion in [purescript-record#55](https://github.com/purescript/purescript-record/issues/55).

The `Record.Builder.merge` and `Record.Builder.union` functions have changed to behave more like `Record.merge` and `Record.union`, in that fields from the argument will override those of the record being built in the case of overlaps. This is a breaking change because the result types may change -- for example, this:

```purs
Builder.build (Builder.merge { x: 1, y: "y" }) { y: 2, z: true }
```

will now produce the type:

```purs
{ x :: Int, y :: String, z :: Boolean }
```

where previously it would produce the type:

```purs
{ x :: Int, y :: Int, z :: Boolean }
```

**To fix:**

If you would like to use the new behavior, then your code doesn't need to change. Otherwise, you can use the new `Record.Builder.flip` function to flip the arguments and recover the old behavior.

```purs
Builder.build (Builder.flip Builder.merge ...)
```

#### The `Data.Either.fromLeft` and `Data.Either.fromRight` functions are now total functions.

Implemented in [purescript-either#48](https://github.com/purescript/purescript-either/pull/48) with additional discussion in [purescript-either#47](https://github.com/purescript/purescript-either/issues/47) and [purescript-either#56](https://github.com/purescript/purescript-either/issues/56).

The `Data.Either.fromLeft` and `Data.Either.fromRight` functions are now total functions and take a default value (like `Data.Maybe.fromMaybe`) instead of partial functions (like `Data.Maybe.fromJust`).

**To fix:**

The most common way to use `fromLeft` and `fromRight` is to unsafely discharge the `Partial` constraint using `unsafePartial` like this:

```purs
x = unsafePartial fromLeft $ Left 10
y = unsafePartial fromRight $ Right "hi"
```

This can be replaced with the new lazy `fromLeft'` and `fromRight'` combined with `unsafeCrashWith`:

```purs
x = fromLeft' (\_ -> unsafeCrashWith "Unexpected Right") $ Left 10
x = fromRight' (\_ -> unsafeCrashWith "Unexpected Left") $ Right "hi"
```

Alternately, you can provide a default value in order to use `fromLeft` and `fromRight` safely.

#### The `quickcheck` library now uses `NonEmptyArray` and `NonEmptyList` instead of `NonEmpty Array` and `NonEmpty List`.

Implemented in [purescript-quickcheck#118](https://github.com/purescript/purescript-quickcheck/pull/118) with additional discussion in [purescript-quickcheck#109](https://github.com/purescript/purescript-quickcheck/issues/109).

The `NonEmpty Array` type predates the `NonEmptyArray` type, which provides O(n) conversion to and from `Array`. The QuickCheck library has switched to use `NonEmptyArray` and `NonEmptyList`, which affects libraries which create generators using functions like `elements` or `frequency`.

**To fix:**

Instead of constructing a `NonEmpty Array` for functions like `Test.QuickCheck.Gen.elements`, you should instead construct a `NonEmptyArray`.

Here's a common way to construct a `NonEmpty Array` in an `Arbitrary` instance:

```purs
import Data.NonEmpty ((:|))

instance arbitraryOrdering :: Arbitrary Ordering where
  arbitrary = elements $ LT :| [ EQ, GT ]
```

This can be replaced by this instance, to minimize the difference between the old code and the new code:

```purs
import Data.Array.NonEmpty (cons')

instance arbitraryOrdering :: Arbitrary Ordering where
  arbitrary = elements $ cons' LT [ EQ, GT ]
```

Or, for a slightly more performant version which constructs the array and converts it without an intermediate `cons`:

```purs
import Data.Array.NonEmpty (fromArray)

instance arbitraryOrdering :: Arbitrary Ordering where
  arbitrary = unsafePartial fromJust $ fromArray [ LT, EQ, GT ]
```


#### The `unicode` library now uses `CodePoint` instead of `Char`.

Implemented in [purescript-unicode#15](https://github.com/purescript-contrib/purescript-unicode/pull/15).

The `Char` type only works with non-astral characters due to its JavaScript representation, whereas the `CodePoint` type represents any unicode character. The Unicode library now uses `CodePoint` for all its string transformations, which is a more correct behavior.

As part of this change, some modules and functions have been renamed.

**To fix:**

If you were importing functions from `Data.Char.Unicode`, rename the import to Data.CodePoint.Unicode` instead:

```diff
- import Data.Char.Unicode
+ import Data.CodePoint.Unicode
```

If you were previously using functions which accepted a `Char` and need to transform that `Char` into a `CodePoint` for compatibility, you should use the `codePointFromChar` function:

```diff
- Data.Char.Unicode.isSeparator '\n'
+ Data.CodePoint.Unicode.isSeparator 
+   (Data.CodePoint.Unicode.codePointFromChar '\n')
```

## Tooling Changes

In PureScript 0.14, compilation errors are printed to stderr instead of stdout ([#3672](https://github.com/purescript/purescript/issues/3672), [#3839](https://github.com/purescript/purescript/pull/3839)). This is the only change in the compiler that affected tools downstream.

### Spago

Spago does not require changes to be compatible with PureScript 0.14. You will, however, want to update to a [0.14-compatible package set](https://github.com/purescript/package-sets) in your `packages.dhall` file.

### Pulp

Pulp does not require changes to be compatible with PureScript 0.14. You will, however, need to update your `bower.json` file to point at new major versions for your dependencies.

### Other tooling

The PureScript 0.14 compiler requires updates to some other commonly-used tools in the PureScript community which are outside the core organization.

#### Update `purescript-psa` to version v0.8.0

The error-reporting frontend [psa](https://github.com/natefaubion/purescript-psa) is compatible with how the 0.14 compiler reports errors as of version v0.8.0 ([#44](https://github.com/natefaubion/purescript-psa/pull/44)) and backwards-compatible with the PureScript 0.13.8 compiler as of v0.8.2 ([#49](https://github.com/natefaubion/purescript-psa/pull/49)).

#### Update `purescript-language-server`

The [PureScript language server](https://github.com/nwolverson/purescript-language-server), which provides IDE support for editors like VSCode, also requires a version bump to be compatible with PureScript 0.14 error reporting.
