By _Phil Freeman_
 
[QuickCheck](http://en.wikipedia.org/wiki/QuickCheck) is a _property-based_ testing library which was originally written in Haskell, but which has been ported to [a](https://github.com/mcandre/node-quickcheck) [number](https://github.com/mcandre/qc) [of](https://github.com/mcandre/cl-quickcheck) [other](https://pypi.python.org/pypi/pytest-quickcheck/) [languages](https://github.com/hayeah/rantly).

QuickCheck works by generating random data with which to test your properties. This allows us to gain confidence that the properties hold, as more and more randomly-generated tests are run.

[`purescript-quickcheck`](http://github.com/purescript/purescript-quickcheck) is a port of QuickCheck to PureScript, which preserves the syntax and types of the original Haskell code.

QuickCheck can be used from PureScript to test code which is also written in PureScript, but can also be used to test Javascript functions as well. In this post, I'll demonstrate each of these use cases.

_Note_: The [`grunt-init-purescript`](http://github.com/purescript-contrib/grunt-init-purescript) Grunt template already contains an example of a QuickCheck-powered test suite, which can be run using Grunt.

## Testing PureScript Functions

QuickCheck defines the `quickCheck` function, which will print test results to the console, or fail with an exception in the event of a test failure. For this demo, I'll use a related function `quickCheckPure`, which is a pure function which can be used from `psci`.

Clone the [`starter-kit`](http://github.com/purescript/starter-kit) project, follow the instructions to build the modules, and load up `psci`.

Start by importing the QuickCheck library:

```
> :i Test.QuickCheck
```

The `quickCheckPure` function takes three arguments: a seed value for the random number generator, the number of tests you would like to run, and the property you would like to test.

Note that the first two arguments are not required when using the `quickCheck` function. The random seed will be chosen randomly, and 100 tests will be run by default.

Let's try a very simple property:

```
> quickCheckPure 12345 10 $ \n -> n + 1 == 1 + n
``` 

If everything worked, you should see the following result:

```
[Success,Success,Success,Success,Success,Success,Success,Success,Success,Success]
```

This indicates ten successful random test runs.

## Error Messages

Let's see what happens when we try testing a broken property:

```
> quickCheckPure 12345 1 $ \n -> n + 1 == n 
```

You should see the following result:

```
Failed: Test returned false
```

That's not a very helpful error, so let's improve it:

```
> quickCheckPure 12345 1 $ \n -> n + 1 == n <?> "Test failed for input " ++ show n
``` 

This time you should see the following failure message:

```
[Failed: Test failed for input 0.000011497177183628082] 
```

## Example 1 - GCD Function

Let's write an implementation of the _greatest common divisor_ function in `psci`:

```
> let
    gcd 0 n = n
    gcd n 0 = n
    gcd n m | n > m = gcd (n - m) m
    gcd n m = gcd n (m - n)
```

Now let's assert some basic properties that we expect to hold of the `gcd` function.

First of all, we need a way to generate random positive integers. The standard random generator for `Number` which comes bundled with `purescript-quickcheck` generates floats in the range `0.0-1.0`. We can generate small integers by using the `Math.round` function:

```
> quickCheckPure 12345 10 $ \n -> 
    let i = Math.round (n * 1000)
    in gcd i 1 == 1

[Success,Success,Success,Success,Success,Success,Success,Success,Success,Success]
```

Generating random integers this way every time will quickly becoming annoying, so let's define a newtype which can be used to generate small integers.

Create a new file `src/Int.purs` and paste the following code:

```
module Int where

import Test.QuickCheck

data Int = Int Number

runInt :: Int -> Number
runInt (Int i) = i

instance arbInt :: Arbitrary Int where
  arbitrary = convert <$> arbitrary
    where
    convert n = Int $ Math.round (n * 100)
```

Back in `psci`, import your new module:

```
> :m src/Int.purs
> :i Int
```

We can now test properties without having to explicitly define how to generate our random data:

```
> quickCheckPure 12345 10 $ \n m ->
    gcd (runInt n) (runInt m) == gcd (runInt m) (runInt n)

Compiling Int
[Success,Success,Success,Success,Success,Success,Success,Success,Success,Success]
```

The idea is that the particular scheme that is chosen to generate data should be _type-directed_, so newtypes can be quite useful when defining multiple data generation schemes for a single type.

## Example 2 - Testing Higher Order Functions

QuickCheck can also be used to test higher-order functions, by randomly generating functions.

Let's test that the `Data.Array.map` function satisfies the functor laws.

For these two tests, I will write the test function using a let binding to avoid having to write type signatures in properties.

The first functor law says that if you map a function which does not modify its argument (the identity function) over a structure, then the structure should not be modified either.

```
> :i Data.Array

> let 
    firstFunctorLaw :: [Number] -> Boolean
    firstFunctorLaw arr = map id arr == arr

> quickCheckPure 12345 10 firstFunctorLaw

[Success,Success,Success,Success,Success,Success,Success,Success,Success,Success]
```

The second functor law says that mapping two functions over a structure one-by-one is equivalent to mapping their composition over the structure:

```
> :i Data.Array

> let
    secondFunctorLaw :: (Number -> Number) -> (Number -> Number) -> [Number] -> Boolean
    secondFunctorLaw f g arr = map f (map g arr) == map (f <<< g) arr
  
> quickCheckPure 12345 10 secondFunctorLaw

[Success,Success,Success,Success,Success,Success,Success,Success,Success,Success]
```

## Testing Javascript Functions

Now let's try an example of testing a function written in Javascript.

[This file](https://gist.github.com/paf31/3aedd6c3e3ac5c8a78e7) contains a set of FFI bindings for some of the functions defined by the [UnderscoreJS](http://underscorejs.org/) library. It is a nice example of a set of pure functions written in Javascript which we can test with QuickCheck.

Copy the contents of that file into `src/UnderscoreFFI.purs`, and reload `psci` with that module loaded:

```
psci src/UnderscoreFFI.purs

> :i UnderscoreFFI
```

The `UnderscoreFFI` module defines a wrapper for the `sortBy` function. Let's test that the function is idempotent:

```
> let 
    sortIsIdempotent :: [Number] -> Boolean
    sortIsIdempotent arr = sortBy id (sortBy id arr) == sortBy id arr
  
> quickCheckPure 12345 10 sortIsIdempotent
  
[Success,Success,Success,Success,Success,Success,Success,Success,Success,Success]
```

In fact, we don't need to sort by the identity function. Since QuickCheck supports higher-order functions, we can test with a randomly-generated sorting function:

```
> let 
    sortIsIdempotent' :: (Number -> Number) -> [Number] -> Boolean
    sortIsIdempotent' f arr = sortBy f (sortBy f arr) == sortBy f arr
  
> quickCheckPure 12345 10 sortIsIdempotent'
  
[Success,Success,Success,Success,Success,Success,Success,Success,Success,Success]
```

Have a look through the `UnderscoreFFI` module, and see what other properties you can define.

## Conclusion

Hopefully I've shown that QuickCheck can be a useful tool, whether you write your code in PureScript or not. Its strength is in its _type-directed_ approach to data generation, which allows you to say _what_ you want to test directly, rather than _how_ to generate test data.