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

That `derive` line replaced all this code:

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

The `Show` type class is most often used for debugging data, so the output of most `Show` instances can be copy-pasted back into a PureScript source file to reconstruct the original data. The `Show` instance we created by deriving `Generic` and then using `genericShow` follows this convention.

This is a good opportunity to emphasize how newtype deriving is different from instances derived by the compiler or through the `Generic` type class. In the examples below, notice how the instance derived through `Generic` includes the newtype constructor `Score`, but the newtype-derived instance simply reuses the underlying `Show` instance for `Int` and therefore does not include the constructor:

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

