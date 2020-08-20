# Type Classes

PureScript supports type classes via the `class` and `instance` keywords.

Types appearing in class instances must be of the form `String`, `Number`, `Boolean`, or `C t1 ... tn` where `C` is a type constructor (including `->` and `t_i` are types of the same form).

Here is an example of the `Show` typeclass, with instances for `String`, `Boolean` and `Array`:

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

Overlapping instances are no longer allowed in PureScript. To write overlapping instances, you should use Instance Chains.

## Instance Chains

PureScript implements a form of instance chains that work on groups of instances matching by parameters. This means that constraints are not considered when choosing instances. However, you can still write a chain of instances in consecutive order that will be matched top to bottom by using the `else` keyword.

Here is an example of a `MyShow` typeclass, with instances for `String`, `Boolean`, and any other type.

```purescript
class MyShow a where
  myShow :: a -> String

instance showString :: MyShow String where
  myShow s = s

else instance showBoolean :: MyShow Boolean where
  myShow true = "true"
  myShow false = "false"

else instance showA :: MyShow a where
  myShow _ = "Invalid"

data MysteryItem = MysteryItem

main = do
  log $ myShow "hello" -- hello
  log $ myShow true -- true
  log $ myShow MysteryItem -- Invalid
```

## Multi-Parameter Type Classes

TODO. For now, see the section in [PureScript by Example](https://book.purescript.org/chapter6.html#multi-parameter-type-classes).

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

Orphan instances are banned because they can lead to incompatible duplicated instances for the same type and class. For example, suppose two separate modules define an orphan `Semigroup Int` instance, and one of them uses `+` for `append`, whereas the other uses `*`. Now suppose someone writes a third module which imports both of the first two, and that somewhere in that third module we have the expression `2 <> 3`, which calls for a `Semigroup Int` instance. The compiler now has two instances to choose from. What should it do? It could report an error, or it could arbitrarily pick one of the instances. Neither option is particularly appealing:

 * If it chooses to report an error, it means that any pair of modules which define the same orphan instance can never be used together.
 * If it arbitrarily picks one, we won't be able to determine whether `2 <> 3` will evaluate to 5 or 6. This can make it very difficult to ensure that your program will behave correctly!

Banning orphan instances also ensures global uniqueness of instances. Without global uniqueness, you risk operating on data with incompatible instances in different sections of code. For example, in Ord-based maps and sets, if it were possible to insert some values into a map using one `Ord` instance, and then try to retrieve them using a different `Ord` instance, you'd have keys disappear from your map. Another example is if you had a type class which defined serialization and deserialization operations, you'd be able to serialize something with one instance and then try to deserialize it with a different incompatible instance.

For multi-parameter type classes, the orphan instance check requires that the instance is either in the same module as the class, or the same module as at least one of the types occurring in the instance. (TODO: example)

## Functional Dependencies

Instances for type classes with multiple parameters generally only need a subset of the parameters to be concrete to match instances. Declarations on which parameters can determine others in instance heads are called Functional Dependencies. For example:

```purescript
class TypeEquals a b | a -> b, b -> a where
  to :: a -> b
  from :: b -> a

instance refl :: TypeEquals a a where
  to a = a
  from a = a
```

The `|` symbol marks the beginning of functional dependencies, which are separated by a comma if there are more than one. In this case, the first parameter determines the type of the second, and the second determines the type of the first.

Functional dependencies are especially useful with the various `Prim` typeclasses, such as `Prim.Row.Cons`: https://pursuit.purescript.org/builtins/docs/Prim.Row#t:Cons

See also the section in [PureScript by Example](https://book.purescript.org/chapter6.html#functional-dependencies).

## Type Class Deriving

The compiler can derive type class instances to spare you the tedium of writing boilerplate. There are a few ways to do this depending on the specific type and class being derived.

### Classes with built-in compiler support

Some classes have special built-in compiler support, and their instances can be derived from all types.

For example, if you you'd like to be able to remove duplicates from an array of an ADT using `nub`, you need an `Eq` and `Ord` instance. Rather than writing these manually, let the compiler do the work.

```purs
import Data.Array (nub)

data MyADT
  = Some
  | Arbitrary Int
  | Contents Number String

derive instance eqMyADT :: Eq MyADT
derive instance ordMyADT :: Ord MyADT

nub [Some, Arbitrary 1, Some, Some] == [Some, Arbitrary 1]
```

Currently, instances for the following classes can be derived by the compiler:
- [Data.Generic.Rep (class Generic)](https://pursuit.purescript.org/packages/purescript-generics-rep/docs/Data.Generic.Rep#t:Generic)
- [Data.Eq (class Eq)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Eq#t:Eq)
- [Data.Ord (class Ord)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Ord#t:Ord)
- [Data.Functor (class Functor)](https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Functor#t:Functor)
- [Data.Newtype (class Newtype)](https://pursuit.purescript.org/packages/purescript-newtype/docs/Data.Newtype#t:Newtype)

### Derive from `newtype`

If you would like your newtype to defer to the instance that the underlying type uses for a given class, then you can use newtype deriving via the `derive newtype` keywords.

For example, let's say you want to add two `Score` values using the `Semiring` instance of the wrapped `Int`.

```purs
newtype Score = Score Int

derive newtype instance semiringScore :: Semiring Score

tenPoints :: Score
tenPoints = (Score 4) + (Score 6)
```

That `derive` line replaced all this code!:

```purs
-- No need to write this
instance semiringScore :: Semiring Score where
  zero = Score 0
  add (Score a) (Score b) = Score (a + b)
  mul (Score a) (Score b) = Score (a * b)
  one = Score 1
```

Note that we can use either of these options to derive an `Eq` instance for a `newtype`, since `Eq` has built-in compiler support. They are equivalent in this case.

```purs
derive instance eqScore :: Eq Score
derive newtype instance eqScore :: Eq Score
```

### Deriving from `Generic`

The compiler's built-in support for `Generic` unlocks convenient deriving for many other classes not listed above.

For example, if we wanted to derive a `Show` instance for `MyADT` it might seem like we're out of luck: `Show` is not a class with built-in compiler support for deriving and `MyADT` is not a `newtype` (so we can't use newtype deriving).

But we _can_ use `genericShow`, which works with _any_ type that has a `Generic` instance. And recall that the compiler has built-in support for deriving a `Generic` instance for any type (including the `MyADT` type). We put all those pieces together like so:

```purescript
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Console (logShow)

derive instance genericMyADT :: Generic MyADT _

instance showMyADT :: Show MyADT where
  show = genericShow
  
main = logShow [Some, Arbitrary 1, Contents 2.0 "Three"]
-- Prints:
-- [Some,(Arbitrary 1),(Contents 2.0 "Three")]
```

Note that the `show` output string can be copy-pasted to reconstruct the original data. This is a good oportunity to emphasize how `newtype` deriving simply reuses the instance of the underlying type. In the case of `show`, this means omitting printing the wrapper.

```purs
import Effect.Console (logShow)

newtype Score = Score Int

-- newtype deriving omits wrapper with show
derive newtype instance showScore :: Show Score

main = logShow (Score 5)
-- Prints:
-- 5
```

```purs
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Console (logShow)

newtype Score = Score Int

-- generic deriving prints wrapper with show
derive instance genericScore :: Generic Score _
instance showScore :: Show Score where
  show = genericShow

main = logShow (Score 5)
-- Prints:
-- (Score 5)
```

More information on Generic deriving is available [in the generics-rep library documentation](https://pursuit.purescript.org/packages/purescript-generics-rep). See this [blog post](https://harry.garrood.me/blog/write-your-own-generics/) for a tutorial on how to write your own `generic` functions.

#### Avoiding stack overflow errors with recursive types

Be careful when using generic functions with recursive data types. Due to strictness, these instances _cannot_ be written in point free style:

```purs
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Console (logShow)

data Chain a
  = End a
  | Link a (Chain a)

derive instance genericChain :: Generic (Chain a) _

instance showChain :: Show a => Show (Chain a) where
  show c = genericShow c -- Note the use of the seemingly-unnecessary variable `c`

main = logShow $ Link 1 $ Link 2 $ End 3
-- Prints:
-- (Link 1 (Link 2 (End 3)))
```

If the instance was written in point free style, then would produce a stack overflow error:

``` purs
instance showChain :: Show a => Show (Chain a) where
  show = genericShow -- This line is problematic

-- Throws this error:
-- RangeError: Maximum call stack size exceeded
```

This technique of undoing point free notation is known as _eta expansion_.

## Compiler-Solvable Type Classes

Some type classes can be automatically solved by the PureScript Compiler without requiring you place a PureScript statement, like `derive instance`, in your source code.

``` purescript
foo :: forall t. (Warn "Custom warning message") => t -> t
foo x = x
```

Automatically solved type classes are included in the [Prim](https://pursuit.purescript.org/builtins/docs/Prim) modules:

Symbol-related classes

- [`IsSymbol`](https://pursuit.purescript.org/packages/purescript-symbols/3.0.0/docs/Data.Symbol#t:IsSymbol)
- [`Append`](https://pursuit.purescript.org/builtins/docs/Prim.Symbol#t:Append)
- [`Compare`](https://pursuit.purescript.org/builtins/docs/Prim.Symbol#t:Compare)
- [`Cons`](https://pursuit.purescript.org/builtins/docs/Prim.Symbol#t:Cons)

[Prim.Row](https://pursuit.purescript.org/builtins/docs/Prim.Row)

- [`Cons`](https://pursuit.purescript.org/builtins/docs/Prim.Row#t:Cons)
- [`Union`](https://pursuit.purescript.org/builtins/docs/Prim.Row#t:Union)
- [`Nub`](https://pursuit.purescript.org/builtins/docs/Prim.Row#t:Nub)
- [`Lacks`](https://pursuit.purescript.org/builtins/docs/Prim.Row#t:Lacks)

[Prim.RowList](https://pursuit.purescript.org/builtins/docs/Prim.RowList)

- [`RowToList`](https://pursuit.purescript.org/builtins/docs/Prim.RowList#t:RowToList)

Other classes

- [`Partial`](https://pursuit.purescript.org/builtins/docs/Prim#t:Partial)
- [`Fail`](https://pursuit.purescript.org/builtins/docs/Prim.TypeError#t:Fail)
- [`Warn`](https://pursuit.purescript.org/builtins/docs/Prim.TypeError#t:Warn)
