## Why have a Partial type class?

Every now and then, you will want to use *partial functions;* that is,
functions which don't handle every possible case of their inputs. For example,
there is a function `fromJust :: ∀ a. Partial ⇒ Maybe a → a` in `Data.Maybe`,
which gives you the value inside a `Just` value, or throws an error if given
`Nothing`.

It's important that types tell the truth wherever possible, because this is a
large part of what allows us to understand PureScript code easily and refactor
it fearlessly.  However, in certain contexts, you know that e.g. an `Either`
value is always going to be `Right`, but you can't prove that to the type
checker, and so you want an escape hatch so that you can write a function that
doesn't have to deal with the `Left` case. This is often the case when
performance is important, for instance.

Previously, partial functions have been indicated by putting the word "unsafe"
at the start of their names, or by putting them in an "Unsafe" module. For
instance, there was previously an `unsafeIndex` function in
`Data.Array.Unsafe`, and `fromJust` used to be in `Data.Maybe.Unsafe`. However,
this is not ideal, because the fact that these functions are partial, and
therefore unsafe if used carelessly, does not appear in the type. Consequently,
there is little to stop you from using it in an inappropriate manner by
accident.

The Partial type class allows us to put this information back into the types,
and thereby allows us to clearly demarcate which parts of your code are
responsible for making that sure unsafe functions are used in a safe manner.

## I just want to use a partial function, please

If you try to just use a partial function, you'll most likely get an error
about no instance being found for the `Partial` class. Take this program, for
instance:

```purescript
module Main where

import Prelude
import Data.Maybe (Maybe(..), fromJust)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = logShow (fromJust (Just 3))
```

Because `fromJust` is partial, and because the partiality hasn't been
explicitly handled, you'll get an error:

```
at src/Main.purs line 8, column 1 - line 8, column 56

  No type class instance was found for

    Prim.Partial
```

*Aside: Yes, this is not a fantastic error. It's going to get better soon.*

The solution is usually to add an application of `unsafePartial`,
from the [Partial.Unsafe module](https://pursuit.purescript.org/packages/purescript-partial), like this:

```purescript
module Main where

import Prelude
import Data.Maybe (Maybe(..), fromJust)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Partial.Unsafe (unsafePartial)

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = logShow (unsafePartial (fromJust (Just 3)))
```

Or, even better than `unsafePartial` is `unsafePartialBecause`, which allows
us to encode *why* we are confidently using this unsafe function.

``` purescript
main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = logShow (unsafePartialBecause "Is hardcoded to be Just." (fromJust (Just 3)))
```

## Where should I put unsafePartial?

The rule of thumb is to use `unsafePartial` at the level of your program such
that the types tell the truth, and the part of your program responsible for
making sure a use of a partial function is safe is also the part where the
`unsafePartial` is. This is perhaps best demonstrated with an example.

Imagine that we want to represent vectors in 3D with an array containing
exactly 3 values (perhaps we want to use them with some other API that expects
this representation, and we don't want to be converting back and forth all the
time). In this case, we would usually use a `newtype` and avoid exporting the
constructor:

```purescript
module Data.V3
  ( V3()
  , makeV3
  , runV3
  ) where

newtype V3 = V3 (Array Number)

makeV3 :: Number -> Number -> Number -> V3
makeV3 x y z = V3 [x, y, z]

runV3 :: V3 -> Array Number
runV3 (V3 v) = v
```

This way, all of the functions are safe; the code will guarantee that any `V3`
does contain exactly 3 values (although the type checker is not aware of this).

Now imagine we want to write a dot product function:

```purescript
dot :: V3 -> V3 -> Number
dot (V3 [x1, x2, x3]) (V3 [y1, y2, y3]) = x1*y1 + x2*y2 + x3*y3
```

We know this is ok, but the compiler disallows it:

```
A case expression could not be determined to cover all inputs.
The following additional cases are required to cover all inputs:

  (V3 _) _
  _      (V3 _)

Alternatively, add a Partial constraint to the type of the enclosing value.

in value declaration dot
```

In this case, we can use `unsafePartialBecause` to explicitly say that we don't
actually need to worry about those other cases, and therefore we don't want to
propagate a `Partial` constraint; users of this `dot` function should not have
to worry about this partiality. For example:

```purescript
dot :: V3 -> V3 -> Number
dot x y = Partial.Unsafe.unsafePartialBecause
    "V3 should have exactly 3 elements, due to non-exported constructor" (go x y)
  where
  go :: Partial => V3 -> V3 -> Number
  go (V3 [x1, x2, x3]) (V3 [y1, y2, y3]) = x1*y1 + x2*y2 + x3*y3
  -- This second pattern can be omitted, but provides a better error message
  -- in case we do get an invalid argument at runtime.
  go _ _ = Partial.crash "Bad argument: expected exactly 3 elements."
```

In this case, we could also use `Partial.Unsafe.unsafeCrashWith`:

```purescript
dot :: V3 -> V3 -> Number
dot (V3 [x1, x2, x3]) (V3 [y1, y2, y3]) = x1*y1 + x2*y2 + x3*y3
dot _ _ = unsafeCrashWith "Bad argument: expected exactly 3 elements."
```

Both implementations will behave in the same way.

In this case, we know our `dot` implementation is fine, and so users of it
should not have to worry about its partiality, so it makes sense to avoid
propagating the constraint. Now, we will see another case where a `Partial`
constraint *should* be propagated.

Let us suppose we want a `foldr1` function, which works in a very similar way
to `foldr` on Lists, except that it doesn't require an initial value to be
passed, and instead requires that the list argument contains at least one
element.

We can implement it like this:

```purescript
foldr1 f (Cons x xs) = foldr f x xs
```

The compiler infers the correct type here, which is:

```purescript
foldr1 :: forall a. Partial => (a -> a -> a) -> List a -> a
```

Now imagine we want a version of `Data.Foldable.minimum` which returns an `a`
instead of a `Maybe a`, and is therefore partial. We can implement it in terms
of our new `foldr1` function:

```purescript
minimumP = foldr1 min
```

Again, the compiler infers the correct type:

```purescript
minimumP :: forall a. (Partial, Ord a) => List a -> a
```

Notice that the `Partial` constraint is automatically propagated to the
`minimumP` function because of the use of another partial function in its
definition, namely `foldr1`. In this case, this is what we want; we should
propagate the `Partial` constraint, because it is still the caller's
responsibility to make sure they supply a non-empty list.

So hopefully it is now clear why this partiality checking is implemented in
terms of a type class: it allows us to elegantly reuse existing machinery in
the type checker in order to check that a Partial constraint is either
explictly handled or propagated. This should help ensure that when you're
reading the code a few months later, it remains clear which part of the code is
responsible for ensuring that any assumed invariants which cannot be encoded in
the type system do hold.
