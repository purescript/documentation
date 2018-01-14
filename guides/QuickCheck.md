---
title: Test your JavaScript with QuickCheck
author: Phil Freeman
published: 2015-07-16
---

[QuickCheck](http://en.wikipedia.org/wiki/QuickCheck) is a _property-based_ testing library which was originally written in Haskell, but which has been ported to [a](https://github.com/mcandre/node-quickcheck) [number](https://github.com/mcandre/qc) [of](https://github.com/mcandre/cl-quickcheck) [other](https://pypi.python.org/pypi/pytest-quickcheck/) [languages](https://github.com/hayeah/rantly).

QuickCheck works by generating random data with which to test your properties. This allows us to gain confidence that the properties hold, as more and more randomly-generated tests are run.

[`purescript-quickcheck`](http://github.com/purescript/purescript-quickcheck) is a port of QuickCheck to PureScript, which preserves the syntax and types of the original Haskell code.

`purescript-quickcheck` can be used to test code which is written in PureScript, but can be used to test Javascript functions as well. In this post, I'll demonstrate each of these use cases.

#### Testing PureScript Functions

QuickCheck defines the `quickCheck` function, which will print test results to the console, or fail with an exception in the event of a test failure.

Create a new project using Pulp, install `purescript-quickcheck`, and open PSCi:

```text
pulp init
bower install purescript-quickcheck
pulp repl
```

Start by importing the QuickCheck library:

```
> import Prelude
> import Test.QuickCheck
```

The `quickCheck` function takes one argument: the property you would like to test. Let's try a very simple property:

```
> quickCheck \n -> n + 1 == 1 + n
```

If everything worked, you should see the following result:

```
100/100 test(s) passed.
unit
```

This indicates 100 successful random test runs.

#### Error Messages

Let's see what happens when we try testing a broken property:

```
> quickCheck \n -> n + 1 == n
```

You should see an exception printed to the console:

```
Error: Test 1 failed:
Failed: Test returned false
```

That's not a very helpful error, so let's improve it:

```
> quickCheck \n -> n + 1 == n <?> "Test failed for input " <> show n
```

This time you should see the following failure message:

```
Error: Test 1 failed:
Test failed for input -654791
```

Alternatively, we could use the `===` operator, which provides a better error message:

```
> quickCheck \n -> n + 1 === n
Error: Test 1 failed:
-663820 /= -663821
```

#### Example 1 - GCD Function

Let's write an implementation of the _greatest common divisor_ function in PSCi.

This function is easiest to enter in the multi-line input mode, so switch to
that by entering `:paste` at the prompt:

```
> :paste
… gcd 0 n = n
… gcd n 0 = n
… gcd n m | n < 0 = gcd (-n) m
… gcd n m | m < 0 = gcd n (-m)
… gcd n m | n > m = gcd (n - m) m
… gcd n m = gcd n (m - n)
…
```

After typing that in, press Control-D to finish the input.

Now let's assert some basic properties that we expect to hold of the `gcd` function.

```
> quickCheck \n -> gcd n 1 === 1
```

This test should pass, but might take a while, because the standard random generator for integers which comes bundled with `purescript-quickcheck` generates integers in the range -1000000 to 1000000.

We can modify our test to only consider small integers:

```
> quickCheck \n -> gcd (n / 1000) 1 === 1
```

This time, the test should complete quickly. However, we've coupled the generation of our data (`/ 1000`) with the property we're testing, which is against the spirit of QuickCheck. A better approach is to define a `newtype` which can be used to generate small integers.

Create a new file `src/SmallInt.purs` and paste the following code:

```
module SmallInt where

import Prelude

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

data SmallInt = SmallInt Int

runInt :: SmallInt -> Int
runInt (SmallInt i) = i

instance arbSmallInt :: Arbitrary SmallInt where
  arbitrary = map (SmallInt <<< (_ / 1000)) arbitrary
```

Back in PSCi, we can now test properties without having to explicitly define how to generate our random data:

```
> quickCheck \(SmallInt n) (SmallInt m) -> gcd n m == gcd m n
```

The idea is that the particular scheme that is chosen to generate data should be indicated by the types of our function arguments, so `newtype`s can be quite useful when defining multiple data generation schemes for a single type.

#### Example 2 - Testing Higher Order Functions

QuickCheck can also be used to test higher-order functions, by randomly generating functions.

Let's test that the `map` function on arrays satisfies the functor laws.

For these two tests, I will write the test function using a let binding to avoid having to write type signatures in properties.

The first functor law says that if you map a function which does not modify its argument (the identity function) over a structure, then the structure should not be modified either.

```
> import Data.Array
> :paste
… firstFunctorLaw :: Array Int -> Boolean
… firstFunctorLaw arr = map id arr == arr
…
> quickCheck firstFunctorLaw
100/100 test(s) passed.
unit
```

The second functor law says that mapping two functions over a structure one-by-one is equivalent to mapping their composition over the structure:

```
> :paste
… secondFunctorLaw :: (Int -> Int) -> (Int -> Int) -> Array Int -> Boolean
… secondFunctorLaw f g arr = map f (map g arr) == map (f <<< g) arr
…
> quickCheck secondFunctorLaw
100/100 test(s) passed.
unit
```

#### Testing Javascript Functions

Now let's try an example of testing a function written in Javascript.

[This file](https://gist.github.com/paf31/3aedd6c3e3ac5c8a78e7) contains a set of FFI bindings for some of the functions defined by the [UnderscoreJS](http://underscorejs.org/) library. It is a nice example of a set of pure functions written in Javascript which we can test with QuickCheck.

Copy the contents of that file into `src/UnderscoreFFI.purs`, and reload PSCi with that module loaded:

```
> import UnderscoreFFI
```

The `UnderscoreFFI` module defines a wrapper for the `sortBy` function. Let's test that the function is idempotent:

```
> :paste
… sortIsIdempotent :: Array Int -> Boolean
… sortIsIdempotent arr = sortBy id (sortBy id arr) == sortBy id arr
…
> quickCheck sortIsIdempotent
100/100 test(s) passed.
unit
```

In fact, we don't need to sort by the identity function. Since QuickCheck supports higher-order functions, we can test with a randomly-generated sorting function:

```
> :paste
… sortIsIdempotent' :: (Int -> Int) -> Array Int -> Boolean
… sortIsIdempotent' f arr = sortBy f (sortBy f arr) == sortBy f arr
…
> quickCheck sortIsIdempotent
100/100 test(s) passed.
unit
```

Have a look through the `UnderscoreFFI` module, and see what other properties you can define.

#### Conclusion

Hopefully I've shown that QuickCheck can be a useful tool, whether you write your code in PureScript or not. Its strength is in its _type-directed_ approach to data generation, which allows you to say _what_ you want to test directly, rather than _how_ to generate test data.
