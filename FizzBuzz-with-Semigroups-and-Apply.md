By _Hardy Jones_

First and foremost, this post is inspired by [this post][fizz-monoid].
You will want to read this post, as it's insightful, short and explains the code that I'm about to borrow.
I noticed an interesting thing while reading that post, so I figured I'd make a post here in PureScriptland.

In Mark's post, he goes on to construct a solution to the fizz buzz question with just the `Maybe` `Monoid`.
A full `Monoid` is needed for two reasons:

1. Haskell doesn't have a `Semigroup` in the base package
1. `mconcat` is used, and that needs an identity element. Although, actually, since the list is never empty, it could've used something like `sconcat` from the [Semigroups][semigroups] package.

In PureScript, we've got a `Semigroup` in the prelude, and I'm going to do away with `mconcat` in exchange for `<>`.

Let's start by converting the Haskell code to PureScript (with a little abstraction over `fizz` and `buzz`).

```purescript
module FizzBuzz where

  import Data.Array
  import Data.Foldable
  import Data.Maybe
  import Data.Monoid

  import Debug.Trace

  fizzBuzz :: Number -> Maybe String
  fizzBuzz = mconcat [fizz, buzz]

  fizz = ifMod 3 "Fizz"
  buzz = ifMod 5 "Buzz"

  ifMod :: Number -> String -> Number -> Maybe String
  ifMod m str n = if n % m == 0 then Just str else Nothing

  main = traverse_ (print <<< go) $ range 1 100
    where go n = maybe (show n) id (fizzBuzz n)
```

With a little help from bower we can run this in node and get the output we expect:

```shell
$ bower i purescript-{arrays,foldable-traversable,maybe,monoid}
...
$ psc fizzbuzz.purs bower_components/purescript-*/src/**/*.purs --main=FizzBuzz | node
"1"
"2"
"Fizz"
"4"
"Buzz"
"Fizz"
"7"
"8"
"Fizz"
"Buzz"
"11"
"Fizz"
"13"
"14"
"FizzBuzz"
...
```

Wonderful, so we can convert some Haskell into PureScript.
Something we should take note of is the instance for `Semigroup` on functions.
When I was playing around with this code, I was reminded of `on` from `Data.Function`.
Its implementation is:

```purescript
on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = g x `f` g y
```

If we replace the `f` by `<>` and allow each side to take their own functions, but use the same input, we see that is exactly the `Semigroup` instance for functions.
Let's look at what type this function would have.

```purescript
forall a b c. (b -> b -> c) -> (a -> b) -> (a -> b) -> a -> c
```

Huh, now that looks awfully similar to `lift2` from `Control.Apply` (modulo some parens).
Let's see its implementation:

```purescript
lift2 :: forall a b c f. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
lift2 f a b = f <$> a <*> b
```

So if we could find an instance of `Apply` that worked for functions, we could just use lift2, or its implemenation.
But there should be an instance of `Apply` for functions, the instance we just showed in the type signature above.
We just need to sub out the `a`'s in `lift2` for `b`'s and fix the `f` instance to `(a ->)` in order to match our wanted type.
We get:

```purescript
lift2' :: forall a b c. (b -> b -> c) -> (a -> b) -> (a -> b) -> (a -> c)
lift2' f a b = f <$> a <*> b
```

Interesting! So we could've used `lift2 (<>)` for our `Semigroup` implementation.
I always find it interesting to see these concepts that seem so disparate can actually be connected.

The next move should come as no surprise.
We're going to replace `mconcat` with an inlined call to `<>`.

```purescript
fizzBuzz = (<>) <$> fizz <*> buzz
```

This works well because it removes the necessity of a `Monoid` constraint.
Well, that's pretty cool.
We managed to sneak in an `Apply` into fizz buzz and drop a `Monoid` instead.

The completed program is:

```purescript
module FB where

  import Data.Array
  import Data.Foldable
  import Data.Maybe

  import Debug.Trace

  fizzBuzz = (<>) <$> fizz <*> buzz

  fizz = ifMod 3 "Fizz"
  buzz = ifMod 5 "Buzz"

  ifMod :: Number -> String -> Number -> Maybe String
  ifMod m str n = if n % m == 0 then Just str else Nothing

  main = traverse_ (print <<< go) $ range 1 100
    where go n = maybe (show n) id (fizzBuzz n)
```

Of course, this change from `mconcat` to `<>` changes how we would extend the program.
In Mark's program, we can extend it by simply adding another function to the list in `fizzBuzz`.
In this program we have to be a bit more cautious, Since `<>` is a binary function.
We can extend it with another function:

```purescript
fizzBuzzBazz = (<>) <$> fizzBuzz <*> bazz
```

So we'd have an extended whole program of (increasing the upper bound so we actually hit the composition of `fizz`, `buzz`, and `bazz`):

```purescript
module FB where

  import Data.Array
  import Data.Foldable
  import Data.Maybe

  import Debug.Trace

  fizzBuzz = (<>) <$> fizz <*> buzz
  fizzBuzzBazz = (<>) <$> fizzBuzz <*> bazz

  fizz = ifMod 3 "Fizz"
  buzz = ifMod 5 "Buzz"
  bazz = ifMod 7 "Bazz"

  ifMod :: Number -> String -> Number -> Maybe String
  ifMod m str n = if n % m == 0 then Just str else Nothing

  main = traverse_ (print <<< go) $ range 1 110
    where go n = maybe (show n) id (fizzBuzzBazz n)
```

This gets a bit hairy if we need to continue extending it, so it's probably better to move on to using the `Monoid` instance as in the original post:

```purescript
module FB where

  import Data.Array
  import Data.Foldable
  import Data.Maybe

  import Debug.Trace

  fizzBuzzBazz = mconcat [fizz, buzz, bazz]

  fizz = ifMod 3 "Fizz"
  buzz = ifMod 5 "Buzz"
  bazz = ifMod 7 "Bazz"

  ifMod :: Number -> String -> Number -> Maybe String
  ifMod m str n = if n % m == 0 then Just str else Nothing

  main = traverse_ (print <<< go) $ range 1 110
    where go n = maybe (show n) id (fizzBuzzBazz n)
```

[fizz-monoid]: http://barkmadley.com/2009/01/22/fun-with-fizzbuzz-and-haskell-monoids.html
[semigroups]: https://hackage.haskell.org/package/semigroups