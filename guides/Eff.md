---
title: Handling Native Effects with the Eff Monad
author: Phil Freeman
published: 2016-05-24
---

In this post, I'm going to talk about PureScript's hybrid approach to handling side-effects.

As in Haskell, values in PureScript do not have side-effects by default, and there are a number of techniques for handling "non-native" side-effects. Such techniques include the use of things like monoids, monads, applicative functors and arrows, but I'm not going to talk about those here. I'm going to talk about how PureScript handles "native" effects, i.e. effects which are provided by the runtime system, and which cannot be emulated by pure functions.

Some examples of native effects are:

- Console IO
- Random number generation
- Exceptions
- Reading/writing mutable state

And in the browser:

- DOM manipulation
- XMLHttpRequest / AJAX calls
- Interacting with a websocket
- Writing/reading to/from local storage

#### The Eff Monad

PureScript's [`purescript-eff`](https://pursuit.purescript.org/packages/purescript-eff/) package defines a monad called `Eff`, which is used to handle native effects. The goal of the `Eff` monad is to provide a typed API for effectful computations, while at the same time generating efficient Javascript.

Let's start with an example:

``` haskell
module RandomExample where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Random (random)
import Control.Monad.Eff.Console (logShow)

printRandom = do
  n <- random
  logShow n
```

This example requires the [`purescript-console`](https://pursuit.purescript.org/packages/purescript-console/) and [`purescript-random`](https://pursuit.purescript.org/packages/purescript-random/) dependencies to be installed:

    pulp init
    bower install --save purescript-console purescript-random

If you save this file as `src/RandomExample.purs`, you will be able to compile and run it using PSCi:

    pulp repl

    > import RandomExample
    > printRandom
    ...

You should see a randomly selected number between 0 and 1 printed to the console.

This program uses `do`-notation to combine two types of native effects provided by the Javascript runtime: random number generation and console IO.

#### Extensible Records and Extensible Effects

We can inspect the type of `printRandom` by using the `:type command`

    > import RandomExample
    > :type printRandom

The type of `printRandom` will be printed to the console. You should see a type which looks like this:

	forall e. Eff (console :: CONSOLE, random :: RANDOM | e) Unit

This type looks quite complicated, but is easily explained by analogy with PureScript's extensible records system.

Consider a simple function which uses extensible records:

``` haskell
fullName person = person.firstName <> " " <> person.lastName
```

This function creates a full name string from an object containing `firstName` and `lastName` properties. If you find the type of this function in PSCi as before, you will see this:

    forall t. { firstName :: String, lastName :: String | t } -> String

The readable version of this type is "`fullName` takes an object with `firstName` and `lastName` properties _and any other properties_ and returns a `String`".

That is, `fullName` does not care if you pass an object with _more_ properties, as long as the `firstName` and `lastName` properties are present:

```
> fullName { firstName: "Phil", lastName: "Freeman", location: "Los Angeles" }
Phil Freeman
```

Similarly, the type of `printRandom` above can be interpreted as follows: "`printRandom` is an effectful computation, which can be run in any environment which supports random number generation and console IO, _and any other types of side effect_, and which yields a value of type `Unit`".

This is the origin of the name "extensible effects": we can always _extend_ the set of side-effects, as long as we can support the set of effects that we need.

#### Interleaving Effects

This extensibility allows code in the `Eff` monad to _interleave_ different types of effects.

The `random` function which we used has the following type:

    forall e1. Eff (random :: RANDOM | e1) Number

which is _not_ the same as the type of `main`.

However, we can instantiate the polymorphic type variable in such a way that the types do match. If we choose `e1 ~ (console :: CONSOLE | e)`, then the two rows are equal, up to reordering.

Similarly, `logShow` has a type which can be instantiated to match the type of `printRandom`:

    forall a e2. Show a => a -> Eff (console :: CONSOLE | e2) Unit

This time we have to choose `e2 ~ random :: RANDOM | e`.

The key is that we don't have to give a type for `printRandom` in order to be able to find these substitutions. `psc` will find a most general type for `printRandom` given the polymorphic types of `random` and `logShow`.

#### Aside: The Kind of Eff

Looking at the [source code](https://github.com/purescript/purescript-eff/blob/master/src/Control/Monad/Eff.purs), you will see the following definition for `Eff`:

```
foreign import Eff :: # ! -> * -> *
```

`*` is the usual kind of types, and `!` is the kind of effects. The `#` kind constructor is used to construct kinds for _rows_, i.e. unordered, labelled collections.

So `Eff` is parameterized by a row of effects, and a return type.

If we were to give a kind to the object type constructor `{ ... }`, it would have kind `# * -> *`. That is, an object type is parameterized by a row of types.

#### Fine-Grained Effects

Type annotations are usually not required when using `Eff`, since rows of effects can be inferred, but they can be used to indicate to the compiler which effects are expected in a computation.

If we annotate the previous example with a _closed_ row of effects:

``` haskell
main :: Eff (console :: CONSOLE, random :: RANDOM) Unit
main = do
  n <- random
  logShow n
```

(note the lack of a type variable here), then we cannot accidentally include a subcomputation which makes use of a different type of effect. This is an advantage of `Eff` over Haskell's more coarsely-grained `IO` monad.

#### Handlers and Actions

Rows of effects can also appear on the left-hand side of a function arrow. This is what differentiates actions like `logShow` and `random` from effect _handlers_.

While actions _add_ to the set of required effects, a handler `subtracts` effects from the set.

Consider `catchException` from the [`purescript-exceptions`](https://pursuit.purescript.org/packages/purescript-exceptions/) package:

``` haskell
catchException
  :: forall a e
   . (Error -> Eff e a)
  -> Eff (err :: EXCEPTION | e) a
  -> Eff e a
```

Note that the type of the effect on the right of the final function arrow requires _fewer_ effects than the effect to its left. Namely, `catchException` _removes_ the `EXCEPTION` effect from the set of required effects.

This is useful, because the type system can be used to delimit portions of code which require a particular effect, and then to wrap that code in a handler, embedding it inside a piece of code which does not use that effect.

For example, we can write a piece of code which uses exceptions, then wrap that code using `catchException` to embed the computation in a piece of code which does not use exceptions.

`purescript-eff` also defines the handler `runPure`, which takes a computation with _no_ side-effects, and safely evaluates it as a pure value:

    type Pure a = Eff () a

    runPure :: forall a. Pure a -> a

For example, we can define a version of the division function for which division by zero results in an exception:

``` haskell
module ErrorsExample where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)

divide :: forall e. Int -> Int -> Eff (err :: EXCEPTION | e) Int
divide _ 0 = throw "Division by zero"
divide n m = pure (n / m)
```

If we have already defined this function, we can use the `runPure` and `catchException` handlers to define a version of `divide` which reports its errors using `Either`:

``` haskell
import Data.Either

dividePure :: Int -> Int -> Either String Int
dividePure n m = runPure (catchException (pure <<< Left <<< message) (Right <$> divide n m))
```

Note that _after_ we use `catchException` to remove the `EXCEPTION` effect, there are no more effects remaining, so we can use `runPure` to evaluate the return value.

#### Defining New Effect Types

New effects can be defined using `foreign import data` just as in the case of types.

Suppose we wanted to define an effect for incrementing a single shared global counter. We simply declare the kind of our new type constructor to be `!`:

```
foreign import data COUNTER :: !
```

We can then use our new effect in an action. Primitive actions are usually defined using the FFI, so it is important to understand the underlying model for `Eff`-based effects.

A computation of type `Eff e a` is implemented in Javascript as a zero-argument function whose body is expected to perform its side effects, before finally returning its result.

We can therefore implement a simple action as follows:

```purescript
foreign import incrCounter :: forall e. Eff (counter :: COUNTER | e) Number
```

and in the corresponding [native module](ffi.md):

```javascript
exports.incrCounter = function() {
  return ++globalCounter;
};
```

Note the type we give to `incrCounter`: we use a polymorphic type to make sure that `Counter` can be interleaved with other effects.

Usually, we wouldn't write a handler for the `Counter` effect, since we have no way to guarantee that the `globalCounter` hasn't been modified. However, if we wanted to provide an unsafe "escape hatch" for `Counter`, we might do so as follows:

```purescript
foreign import unsafeRunCounter :: forall e a. Eff (counter :: COUNTER | e) a -> Eff e a
```

And in JavaScript:

```javascript
exports.unsafeRunCounter = function(f) {
  return f;
};
```

#### The Eff Monad is Magic

The `psc` compiler has special support for the `Eff` monad. Ordinarily, a chain of monadic binds might result in poor performance when executed in Node or in the browser. However, the compiler can generate code for the `Eff` monad without explicit calls to the monadic bind function `>>=`.

Take the random number generation from the start of the post. If we compile this example without optimizations, we end up the following Javascript:

``` javascript
var main =
  Prelude[">>="]
    (Control_Monad_Eff.monadEff())
	(Control_Monad_Eff_Random.random)
	(function (n) {
      return Control_Monad_Eff_Console.logShow(Prelude.showNumber())(n);
    });
```

However, if we use the default optimizations, the calls to `Eff`'s monadic bind function are inlined, resulting the following tidier Javascript:

``` javascript
var main = function __do() {
  var n = Control_Monad_Eff_Random.random();
  return Control_Monad_Eff_Console.logShow(Prelude.showNumber())(n)();
};
```

While this is a small improvement, the benefit is greater when using multiple nested calls to `>>=`.

The improvement is even more marked when optimizations are used in conjunction with tail call elimination. Consider the following recursive program which prints an increasing sequence of numbers to the console:

``` haskell
go n = do
  logShow n
  go (n + 1)

main = go 1
```

Without optimizations, the compiler generates the following Javascript, which fails after a few iterations with a stack overflow:

``` javascript
var go =
  Prelude[">>="]
    (Control_Monad_Eff.monadEff())
	(Control_Monad_Eff_Console.logShow(Prelude.showNumber())(n))
	(function (_) {
      return go(n + 1);
    });
```

However, with optimizations, the Javascript can be made to run without errors:

``` javascript
var go = function (__copy_n) {
  return function __do() {
    var n = __copy_n;
    tco: while (true) {
      Control_Monad_Eff_Console.logShow(Prelude.showNumber())(n)();
      var __tco_n = n + 1;
      n = __tco_n;
      continue tco;
    };
  };
};
```

#### Efficient Mutation with ST

The `psc` compiler has additional support for one particular native effect, namely the `ST` effect, which is used to provide scoped mutable state.

Consider the following function, which computes the total stopping time of the Collatz sequence for a given initial value:

```haskell
collatz :: Int -> Int
collatz n = pureST do
  r <- newSTRef n
  count <- newSTRef 0
  untilE do
    modifySTRef count (_ + 1)
    m <- readSTRef r
    writeSTRef r $ if m `mod` 2 == 0 then m / 2 else 3 * m + 1
    pure (m == 1)
  readSTRef count
```

In this case, `psc` notices that the mutable variables `r` and `count` are scoped by `runST` and so can safely be turned into local mutable variables.

The resulting Javascript is surprisingly short:

```javascript
var collatz = function (n) {
  return Control_Monad_Eff.runPure(function __do() {
    var r = n;
    var count = 0;
    (function () {
      while (!(function __do() {
        count = 1 + count;
        var m = r;
        r = (m % 2 === 0) ? m / 2 : 3 * m + 1;
        return m === 1;
      })()) { };
      return {};
    })();
    return count;
  });
};
```

#### Conclusion

The `Eff` monad provides a way to use native effects in PureScript, in such a way that different types of effects can be interleaved, and such that the generated Javascript is relatively simple.
