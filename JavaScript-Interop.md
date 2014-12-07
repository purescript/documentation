By _Phil Freeman_

In the last post, I promised to describe how to interact with Javascript code using the foreign function interface, or FFI. In this short post, I'll go over how to call Javascript functions using the FFI, as well as how to call PureScript functions from Javascript.

### Disclaimer

It should be said that choosing to work with Javascript via the FFI will "void the warranty" of the typechecker to a certain extent. Once you step outside the safe confines of the PureScript type system, nothing is guaranteed, so it is recommended that you know a few basic internals of the language implementation before writing your own FFI bindings. That said, the correspondence between PureScript types and their Javascript representations is fairly simple, so it should not be too difficult to understand.

### Calling PureScript from Javascript

Calling a PureScript function from Javascript is very simple, at least for functions with simple types.

Let's take the following simple module as an example:

``` haskell
module Test where

gcd :: Number -> Number -> Number
gcd n m | n == 0 = m
gcd n m | m == 0 = n
gcd n m | n > m = gcd (n - m) m
gcd n m = gcd (m - n) n
```

This function finds the greatest common divisor of two numbers by repeated subtraction. It is a nice example of a case where you might like to use PureScript to define the function, but have a requirement to call it from Javascript: it is simple to define this function in PureScript since it is made up of pattern matches and recursion, and the implementor can benefit from the use of the type checker.

To understand how this function can be called from Javascript, it is important to realise that PureScript functions always get turned into Javascript functions _of a single argument_, so we need to apply its arguments one-by-one:

``` javascript
// In Javascript
var test = Test.gcd(15)(20);
```

Here, I am assuming that the code was compiled with `psc`, which combines all modules into a single file, using objects in the global namespace to represent modules. For that reason, I was able to reference the `gcd` function on the `Test` object, since `Test` was the name of the module I compiled.

If instead you would like to compiled with `psc-make`, your modules will be compiled to CommonJS modules and placed in the `output` folder by default. In NodeJS, if you copy these generated modules into your `node_modules` directory, you will be able to reference the module by using `require`:

``` javascript
var Test = require('Test');
var test = Test.gcd(15)(20);
```

### Understanding Name Generation

PureScript aims to preserve names during code generation as much as possible. In particular, most identifiers which are neither PureScript nor Javascript keywords can be expected to be preserved, at least for names of top-level declarations.

If you decide to use a Javascript keyword as an identifier, the name will be escaped with a double dollar symbol. For example,

``` haskell
null = []
```

generates the following Javascript:

``` javascript
var $$null = [];
```

In addition, if you would like to use special characters in your identifer names, they will be escaped using a single dollar symbol. For example,

``` haskell
example' = 100
```

generates the following Javascript:

``` javascript
var example$prime = 100;
```

This scheme also applies to names of infix operators:

``` haskell
(%) a b = ...
```

generates

``` javascript
var $percent = function(a) { ... }
```

The full list of escaped characters and their Javascript representations can be found [in the compiler source](https://github.com/purescript/purescript/blob/master/src/Language/PureScript/CodeGen/Common.hs).

### Handling Constrained Types

One special case that you should be aware of when calling PureScript functions from Javascript is that values with constrained types (i.e. types which contain type class constraints) contain extra parameters which are used to pass type class dictionaries to the function.

For example, let's write a simple PureScript function with a constrained type, and look at the generated Javascript.

``` haskell
module Test where

import Data.Tuple

inOrder :: forall a. (Ord a) => a -> a -> Tuple a a
inOrder a1 a2 | a1 < a2 = Tuple a1 a2
inOrder a1 a2 = Tuple a2 a1
```

The generated Javascript looks like this:

``` javascript
var inOrder = function (__dict_Ord_32) {
  return function (_1) {
    return function (_2) {
      if (Prelude["<"](__dict_Ord_32)(_1)(_2)) {
        return Data_Tuple.Tuple(_1)(_2);
      };
      return Data_Tuple.Tuple(_2)(_1);
    };
  };
};
```

Notice that `inOrder` is a (curried) function of three arguments, not two. The first argument is the type class dictionary for the `Ord` constraint.

We can call this function from Javascript by passing an explicit type class dictionary from the Prelude as the first parameter:

``` javascript
var test = Test.inOrder(Prelude.ordNumber())(20)(10);
```

### Calling Javascript from PureScript

Javascript values and functions can be used from PureScript by using the foreign function interface (FFI). The problem becomes how to choose suitable types for values originating in Javascript.

The general rule regarding types is that you can enforce as little or as much type safety as you like when using the FFI, but you should be careful to avoid common pitfalls when dealing with Javascript values, like the /\possiblity of null or undefined values being returned from a Javascript function. Functions defined in the Prelude and core libaries tend to err on the side of type safety where possible.

### Referencing Javascript Values Directly

The simplest way to make a Javascript value available in PureScript is to provide a type using a `foreign import` declaration. If the value or function is sufficiently pure, you may be able to use it directly.

Suppose you had the following function defined in Javascript:

``` javascript
function interestImpl1(amount) {
  return amount * 0.1;
}
```

You would be able to make this function available in PureScript as follows:

``` haskell
foreign import interestImpl1 :: Number -> Number
```

Note that a few restrictions apply:

- Javascript functions can normally only be typed in PureScript if they take exactly one argument.
- Depending on how you would like to manage side-effects, you may wish to provide a wrapper function instead (see the next section).
- In order to handle `null` and `undefined` correctly, as well as other corner cases, you may wish to use the `Data.Foreign` module or another technique to sanitize input data.

### Wrapping Javascript Values

For one or more reasons, it might be preferable to define a wrapper function to call a Javascript function, instead of calling it directly. This is the approach which is taken in a lot of PureScript's core libraries.

For example, you might want to:

- Introduce a wrapper function to handle functions of many arguments correctly.
- Track side-effects using the `Eff` monad.
- Define custom logic for handling erroneous data.

Wrapper functions can be defined inline, by including a string literal in the `foreign import` declaration.

For example, consider the following Javascript function:

``` javascript
function interestImpl2(amount, months) {
  return amount * Math.exp(months * 0.1);
}
```

This function cannot be given a type directly in PureScript, but we can write a wrapper function with a curried function type:

``` haskell
foreign import interestImpl2 
  """
  function interestImpl2(amount) {
    return function(months) {
      return interestImpl2(amount, months);
    };
  }""" :: Number -> Number -> Number
```

### Santizing Foreign Data With Data.Foreign

Data returned from Javascript functions cannot generally be trusted to be defined and non-null. PureScript functions in the Prelude and common libraries generally assume that values will be neither `undefined` nor `null`, so it is important to sanitize data when working with values returned from Javascript functions using the FFI.

The `Data.Foreign` module (available as `purescript-foreign` from the Bower registry) defines a `Foreign` data type, and several helper functions for turning `Foreign` values into regular PureScript values, as well as support for handling `null` and `undefined` using the `Maybe` type constructor.

### Defining Foreign Data Types

It is often useful when wrapping Javascript APIs to create new types at a specific kind for use with the FFI.

For example, suppose we have a Javascript library `frob` which defines the `Frob` data structure and associated functions. To give meaningful types to those functions, it might be useful to define a type `Frob` at kind `*`. We can do this as follows:

``` haskell
foreign import data Frob :: *
```

The type `Frob` can now be used in other types, or in foreign import declarations:

``` haskell
foreign import makeFrob :: String -> Frob
```

### Defining Foreign Type Class Instances

It is also possible to bring type class instances into scope, whose dictionaries are defined in Javascript.

This feature was added in order to support separate compilation, and so is not considered a supported feature unlike ordinary usage of the FFI, but is documented here for completeness.

Suppose we have already defined a foreign data type `Frob` and would like to define an instance of the `Show` typeclass in Javascript.

We might write the type class dictionary as follows:

``` javascript
var showFrob = function() {
  return {
    __superclasses: {},
    show: function(frob) {
      return frob.foo + frob.bar + frob.baz;
    }
  };
};
```

We could then make the instance available in PureScript using the following declaration:

``` haskell
foreign import instance showFrob :: Show Frob
``` 

Note that the same could be accomplished by defining a function representing the `show` function in Javascript, importing that using the FFI, and then writing the `Show Frob` instance in PureScript as a regular type class instance.

### Conclusion

I have hopefully shown that interoperating with Javascript is simple in both directions, once a few small implementation details are understood. You should now be able to wrap your Javascript libraries for use in PureScript, and vice versa.