By _Phil Freeman_

In this post, I'm going to talk about PureScript's hybrid approach to handling side-effects. As in Haskell, values in PureScript do not have side-effects by default, and there are a number of techniques for handling "non-native" side-effects. Such techniques include the use of things like monoids, monads, applicative functors and arrows, but I'm not going to talk about those here. I'm going to talk about how PureScript handles "native" effects, i.e. effects which are provided by the runtime system, and which cannot be emulated by pure functions.

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

### The Eff Monad

PureScript's `Prelude` defines a special monad called `Eff`, which is used to handle native effects. The goal of the `Eff` monad is to provide a typed API for effectful computations, while at the same time generating efficient Javascript.

Let's start with an example:

``` haskell
module RandomExample where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Debug.Trace

main = do
  n <- random
  print n
```

If you save this file as `RandomExample.purs`, you will be able to compile and run it using the following command:

    psc --main RandomExample RandomExample.purs | node

You should see a randomly selected number between 0 and 1 printed to the console.

This program uses `do`-notation to combine two types of native effects provided by the Javascript runtime: random number generation and console IO.

### Extensible Records and Extensible Effects

We can inspect the type of `main` by opening the module in `psci`:

    psci RandomExample.purs
	
    > :t RandomExample.main

The type of `main` will be printed to the console. You should see a type which looks like this:

	forall t. Control.Monad.Eff.Eff (trace :: Debug.Trace.Trace, random :: Random.Random | t) {}

This type looks quite complicated, but is easily explained by analogy with PureScript's extensible records system.

Consider a simple function which uses extensible records:

``` haskell
fullName person = person.firstName ++ " " ++ person.lastName
```

This function creates a full name string from an object containing `firstName` and `lastName` properties. If you find the type of this function in `psci` as before, you will see this:

    forall t. { firstName :: String, lastName :: String | t } -> String 

The readable version of this type is "`fullName` takes an object with `firstName` and `lastName` properties _and any other properties_ and returns a `String`".

That is, `fullName` does not care if you pass an object with _more_ properties, as long as the `firstName` and `lastName` properties are present:

```
> firstName { firstName: "Phil", lastName: "Freeman", location: "Los Angeles" }
Phil Freeman
```

Similarly, the type of `main` above can be interpreted as follows: "`main` is a side-effecting computation, which can be run in any environment which supports random number generation and console IO, _and any other types of side effect_, and which yields a value of type `{}`".

This is the origin of the name "extensible effects": we can always _extend_ the set of side-effects, as long as we can support the set of effects that we need.

### Interleaving Effects

This extensibility allows code in the `Eff` monad to _interleave_ different types of effects.

The `random` function which we used has the following type:

    forall e. Eff (random :: Random | e) Number

which is _not_ the same as the type of `main`.

However, we can instantiate the polymorphic type variable in such a way that the types do match. If we choose `e ~ trace :: Trace | t`, then the two rows are equal, up to reordering.

Similarly, `trace` has a type which can be instantiated to match the type of `main`:

    forall e. String -> Eff (trace :: Trace | e) {}

This time we have to choose `e ~ random :: Random | t`.

The key is that we don't have to give a type for `main` in order to be able to find these substitutions. `psc` will find a most general type for `main` given the polymorphic types of `random` and `trace`.

### Aside: The Kind of Eff

Looking at the [source code of the Prelude](https://github.com/purescript/purescript/blob/master/prelude/prelude.purs), you will see the following definition for `Eff`:

```
foreign import Eff :: # ! -> * -> *
``` 

`*` is the usual kind of types, and `!` is the kind of effects. The `#` kind constructor is used to construct kinds for _rows_, i.e. unordered, labelled collections.

So `Eff` is parameterised by a row of effects, and a return type.

If we were to give a kind to the object type constructor `{ ... }`, it would have kind `# * -> *`. That is, an object type is parameterised by a row of types.

### Fine-Grained Effects

Type annotations are usually not required when using `Eff`, since rows of effects can be inferred, but they can be used to indicate to the compiler which effects are expected in a computation.

If we annotate the previous example with a _closed_ row of effects:

``` haskell
main :: Eff (trace :: Trace, random :: Random) {}
main = do
  n <- random
  print n
```

(note the lack of a type variable here), then we cannot accidentally include a subcomputation which makes use of a different type of effect. This is an advantage of `Eff` over Haskell's more coarsely-grained `IO` monad.

### Handlers and Actions

Rows of effects can also appear on the left-hand side of a function arrow. This is what differentiates actions like `trace` and `random` from effect _handlers_.

While actions _add_ to the set of required effects, a handler `subtracts` effects from the set.

Consider `catchError` from the `Prelude`:

``` haskell
catchError :: forall e r a. (e -> Eff r a) -> Eff (err :: Error e | r) a -> Eff r a
```

Note that the type of the effect on the right of the final function arrow requires _fewer_ effects than the effect to its left. Namely, `catchError` _removes_ the `Error e` effect from the set of required effects.

This is useful, because the type system can be used to delimit portions of code which require a particular effect, and then to wrap that code in a handler, embedding it inside a piece of code which does not use that effect.

For example, we can write a piece of code which uses exceptions, then wrap that code using `catchError` to embed the computation in a piece of code which does not use exceptions.

The `Prelude` also defines the handler `runPure`, which takes a computation with _no_ side-effects, and safely evaluates it as a pure value:

    type Pure a = forall e. Eff e a
	
    runPure :: forall a. Pure a -> a

For example, we can define a version of the division function for which division by zero results in an exception:

``` haskell
module ErrorsExample where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Error

divide :: forall e. Number -> Number -> Eff (err :: Error String | e) Number
divide _ 0 = throwError "Division by zero"
divide n m = return (n / m)
```

If we have already defined this function, we can use the `runPure` and `catchError` handlers to define a version of `divide` which reports its errors using `Data.Either`:

``` haskell
import Data.Either

dividePure :: Number -> Number -> Either String Number
dividePure n m = runPure (catchError (return <<< Left) (Right <$> divide n m))
```

Note that _after_ we use `catchError` to remove the `Error` effect, there are no more effects remaining, so we can use `runPure` to evaluate the return value.

### Defining New Effect Types

New effects can be defined using `foreign import data` just as in the case of types.

Suppose we wanted to define an effect for incrementing a single shared global counter. We simply declare the kind of our new type constructor to be `!`: 

```
foreign import data Counter :: !
```

We can then use our new effect in an action. Primitive actions are usually defined using the FFI, so it is important to understand the underlying model for `Eff`-based effects.

A computation of type `Eff e a` is implemented in Javascript as a zero-argument function whose body is expected to perform its side effects, before finally returning its result.

We can therefore implement a simple action as follows:

```
foreign import incrCounter
  "function incrCounter() { \
  \    return ++globalCounter; \
  \}" :: forall e. Eff (counter :: Counter | e) Number
```

Note the type we give to `incrCounter`: we use a polymorphic type to make sure that `Counter` can be interleaved with other effects.

Usually, we wouldn't write a handler for the `Counter` effect, since we have no way to guarantee that the `globalCounter` hasn't been modified. However, if we wanted to provide an unsafe "escape hatch" for `Counter`, we might do so as follows:

```
foreign import runCounter
  "function runCounter(f) { \
  \    return f; \
  \}" :: forall e. Eff (counter :: Counter | e) a -> Eff e a
```

### The Eff Monad is Magic

The `psc` compiler has special support for the `Eff` monad. Ordinarily, a chain of monadic binds might result in poor performance when executed in `nodejs` or in the browser. However, the compiler can generate code for the `Eff` monad without explicit calls to the monadic bind function `>>=`.

Take the random number generation from the start of the post. If we compile this example using the default options, we end up the following Javascript:

``` javascript
var main = 
  _ps.Prelude[">>="]
    (_ps.Control_Monad_Eff.monadEff({}))
	(_ps.Random.random)
	(function (n) {
      return _ps.Debug_Trace.print(_ps.Prelude.showNumber({}))(n);
    });
```

However, if we use the `--magic-do` compiler option, the calls to `Eff`'s monadic bind function are inlined, resulting the following tidier Javascript:

``` javascript
var main = function __do() {
  var n = _ps.Random.random();
  return _ps.Debug_Trace.print(_ps.Prelude.showNumber({}))(n)();
};
```

While this is a small improvement, the benefit is greater when using multiple nested calls to `>>=`.

The improvement is even more marked when `--magic-do` is used in conjunction with the tail call elimination compiler option `--tco`. Consider the following recursive program which prints an increasing sequence of numbers to the console:

``` haskell
go n = do
  print n
  go (n + 1)

main = go 1
```

The default compiler options yield the following Javascript, which fails after a few iterations with a stack overflow:

``` javascript
var go = function (n) {
  return _ps.Prelude[">>="]
    (_ps.Control_Monad_Eff.monadEff({}))
	(_ps.Debug_Trace.print(_ps.Prelude.showNumber({}))(n))
	(function (_) {
      return go(n + 1);
    });
};
```

However, using the additional options `--magic-do --tco`, the Javascript can be made to run without errors:

``` javascript
var go = function (__copy_n) {
  return function __do() {
    var n = __copy_n;
    tco: while (true) {
      _ps.Debug_Trace.print(_ps.Prelude.showNumber({}))(n)();
      var __tco_n = n + 1;
      n = __tco_n;
      continue tco;
    };
  };
};
```

### Efficient Mutation with ST

The `psc` compiler has additional support for one particular native effect, namely the `ST` effect, which is used to provide scoped mutable state.

Consider the following function, which computes the total stopping time of the Collatz sequence for a given initial value:

```haskell
collatz :: Number -> Number
collatz n = pureST do
  r <- newSTRef n
  count <- newSTRef 0
  untilE $ do
    modifySTRef count $ (+) 1
    m <- readSTRef r
    writeSTRef r $ if m % 2 == 0 then m / 2 else 3 * m + 1
    return $ m == 1
  readSTRef count
```

In this case, `psc` notices that the mutable variables `r` and `count` are scoped by `runST` and so can safely be turned into local mutable variables.

The resulting Javascript is surprisingly short:

```javascript
var collatz = function (n) {
  return _ps.Control_Monad_Eff.runPure(function __do() {
    var r = n;
    var count = 0;
    (function () {
      while (!(function __do() {
        count = 1 + count;
        var m = r;
        r = (m % 2 === 0) ? m / 2 : 3 * m + 1;
        return m === 1;
      })()) {
	  };
      return {};
    })();
    return count;
  });
};
``` 

### Conclusion

The `Eff` monad provides a way to use native effects in PureScript, in such a way that different types of effects can be interleaved, and such that the generated Javascript is relatively simple.

Next, I'll write a post which shows how to use `Eff` to wrap a real-world API.
