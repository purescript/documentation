---
title: The Foreign Function Interface
author: Phil Freeman
published: 2015-07-15
---

In this short post, I'll show how to interoperate with Javascript code using PureScript's Foreign Function Interface (or _FFI_). We'll see how to call Javascript code from PureScript code and vice versa.

#### Disclaimer

It should be said that choosing to work with Javascript via the FFI will "void the warranty" of the typechecker to a certain extent. Once you step outside the safe confines of the PureScript type system, nothing is guaranteed, so it is recommended that you know a few basics of the language implementation before writing your own FFI bindings. That said, the correspondence between PureScript types and their Javascript representations is fairly simple, so it should not be too difficult to understand.

#### Calling PureScript from Javascript

Calling a PureScript function from Javascript is very simple, at least for functions with simple types.

Let's take the following simple module as an example:

``` haskell
module Test where

import Prelude

gcd :: Int -> Int -> Int
gcd n m | n == 0 = m
gcd n m | m == 0 = n
gcd n m | n > m = gcd (n - m) m
gcd n m = gcd (m - n) n
```

This function finds the greatest common divisor of two numbers by repeated subtraction. It is a nice example of a case where you might like to use PureScript to define the function, but have a requirement to call it from Javascript: it is simple to define this function in PureScript since it is made up of pattern matches and recursion, and the implementor can benefit from the use of the type checker.

To understand how this function can be called from Javascript, it is important to realize that PureScript functions always get turned into Javascript functions _of a single argument_, so we need to apply its arguments one-by-one:

``` javascript
var Test = require('Test');
Test.gcd(15)(20);
```

Here, I am assuming that the code was compiled with `psc`, which compiles PureScript modules to CommonJS modules. For that reason, I was able to reference the `gcd` function on the `Test` object, after importing the `Test` module using `require`.

You might also like to bundle JavaScript code for the browser, using `purs bundle`. In that case, you would access the `Test` module on the global namespace, which defaults to `PS`:

``` javascript
var Test = PS.Test;
Test.gcd(15)(20);
```

#### Understanding Name Generation

PureScript aims to preserve names during code generation as much as possible. In particular, most identifiers which are neither PureScript nor Javascript keywords can be expected to be preserved, at least for names of top-level declarations.

If you decide to use a Javascript keyword as an identifier, the name will be escaped with a double dollar symbol. For example,

``` haskell
null = []
```

generates the following Javascript:

``` javascript
var $$null = [];
```

In addition, if you would like to use special characters in your identifier names, they will be escaped using a single dollar symbol. For example,

``` haskell
example' = 100
```

generates the following Javascript:

``` javascript
var example$prime = 100;
```

#### Calling Javascript from PureScript

Javascript values and functions can be used from PureScript by using the FFI. The problem becomes how to choose suitable types for values originating in Javascript.

The general rule regarding types is that you can enforce as little or as much type safety as you like when using the FFI, but you should be careful to avoid common pitfalls when dealing with Javascript values, like the possibility of null or undefined values being returned from a Javascript function. Functions defined in the Prelude and core libraries tend to err on the side of type safety where possible.

#### Foreign Modules

In PureScript, JavaScript code is wrapped using a _foreign module_. A foreign module is just a CommonJS module which is associated with a PureScript module. Foreign modules are required to adhere to certain conventions:

- The name of the foreign module must be the same as its companion PureScript module, with its extension changed to `.js`. This associates the foreign module with the PureScript module.
- All exports must be of the form `exports.name = value;`, specified at the top level.

Here is an example, where we export a function which computes interest amounts from a foreign module:

```javascript
"use strict";

exports.calculateInterest = function(amount) {
  return amount * 0.1;
};
```

This file should be saved as `src/Interest.js`. The corresponding PureScript module `Interest` will be saved in `src/Interest.purs`, and will look like this:

```purescript
module Interest where

foreign import calculateInterest :: Number -> Number
```

In the companion PureScript module, we simply assign a type to the exports of the foreign module by using a `foreign import` declaration. These values are then available to modules which `import` our PureScript module.

#### Functions of Multiple Arguments

PureScript functions are curried by default, so Javascript functions of multiple arguments require special treatment.

Suppose we wanted to modify our `calculateInterest` function to take a second argument:

```javascript
"use strict";

exports.calculateInterest = function(amount, months) {
  return amount * Math.exp(0.1, months);
};
```

A correct `foreign import` declaration now should use a foreign type whose runtime representation correctly handles functions of multiple arguments. The `purescript-functions` package provides a collection of such types for function arities from 0 to 10:

```purescript
module Interest where

import Data.Function.Uncurried (Fn2)

foreign import calculateInterest :: Fn2 Number Number Number
```

Here, the `Fn2` type constructor is used to wrap Javascript functions of two arguments. We can write a curried wrapper function in PureScript which will allow partial application:

```purescript
calculateInterestCurried :: Number -> Number -> Number
calculateInterestCurried = runFn2 calculateInterest
```

An alternative is to use curried functions in the native module, using multiple nested functions, each with a single argument, as the runtime representation of the function type constructor `(->)` dictates:

```javascript
"use strict";

exports.calculateInterest = function(amount) {
  return function(months) {
    return amount * Math.exp(0.1, months);
  };
};
```

This time, we can assign the curried function type directly:

```purescript
foreign import calculateInterest :: Number -> Number -> Number
```

#### Don't call functions with constrained types from JavaScript

It's not safe to call functions that have typeclass constraints from JavaScript, as their representation is unstable in the face of compiler optimizations. If you want to call a PureScript function from JavaScript you need to take all arguments explicitly.

For implementing PureScript functions that rely on typeclass constraints via the FFI refer to [the FFI-Tips document](./FFI-Tips.md#avoid-directly-calling-ps-code-from-foreign-modules).

#### Handling Side Effects

Notice that the `calculateInterest` functions defined above were _pure_: they had no side-effects and produced the same result for the same input on every invocation.

The PureScript function type `a -> b` does not allow for side-effects, so it would be incorrect to assign a function type to a Javascript computation with side-effects. The correct approach in this case is to use the `Effect` type constructor, defined in the `purescript-effect` package, to assign a type to the computation.

The `Effect` type constructor and its usage is documented [on Pursuit](https://pursuit.purescript.org/packages/purescript-effect).

#### Santizing Foreign Data With Data.Foreign

Data returned from Javascript functions cannot generally be trusted to be defined and non-null. PureScript functions in the Prelude and common libraries generally assume that values will be neither `undefined` nor `null`, so it is important to sanitize data when working with values returned from Javascript functions using the FFI.

The `Data.Foreign` module (available in the `purescript-foreign` package) defines a `Foreign` data type, and several helper functions for turning `Foreign` values into regular PureScript values, as well as support for handling `null` and `undefined` using the `Maybe` type constructor.

#### Defining Foreign Data Types

It is often useful when wrapping Javascript APIs to create new types at a specific kind for use with the FFI.

For example, suppose we have a Javascript library `frob` which defines the `Frob` data structure and associated functions. To give meaningful types to those functions, it might be useful to define a type `Frob` at kind `Type`. We can do this as follows:

``` haskell
foreign import data Frob :: Type
```

The type `Frob` can now be used in other types, or in foreign import declarations:

``` haskell
foreign import makeFrob :: String -> Frob
```

Developers who define their own foreign data types should take care to document their expected runtime representations.

#### Conclusion

I have hopefully shown that interoperating with Javascript is simple in both directions, once a few small implementation details are understood. You should now be able to wrap your Javascript libraries for use in PureScript, and vice versa.

The [PureScript book](https://leanpub.com/purescript/read#leanpub-auto-the-foreign-function-interface) contains more information on the FFI, and plenty of examples and exercises for any interested readers.
