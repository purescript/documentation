### Implementing the constrain

Suppose that you wish to have a safe foreign function that can only be used with instances of a given typeclass. For example:

```purescript
-- PureScript code
module ComputerTools where

class Computer f where
  compute :: f -> Int

foreign import triplicate :: forall c. (Computer c) => c -> Int

--

data Box = Box Int

instance computerBox :: Computer Box where
  compute (Box x) = x
```

Though `triplicate` takes only one argument in PureScript side, its definition in foreign side needs one more: a `Computer` type-class dictionary ([PureScript by Example 10.9](https://leanpub.com/purescript/read#leanpub-auto-representing-constrained-types))

```js
//Foreign code

exports.triplicate = function(aComputerDictionary) {
  return function(aComputerInstance) {
    return 3*aComputerDictionary.compute(aComputerInstance);
  };
};
```
### Alternative

If you don't like relying on the runtime representation of type class dictionaries there is another option: you just should pass in the functions explicitly. Then you’d define a wrapper that actually takes the constraint. 
So instead of 

```purescript
foreign import triplicate :: forall c. (Computer c) => c -> Int
```

you would do

```purescript

-- This function is unsafe:
--   c should be a Computer instance
--   but the type checker can't verify this

foreign import triplicateImpl :: forall a. (a -> Int) -> a -> Int   
                                                                    
-- now triplicate is not foreign
triplicate :: forall c. (Computer c) => c -> Int
triplicate = triplicateImpl compute
```

and then you’d have a non-foreign wrapper, that actually has the constraint, and you’d just pass in `compute` to the foreign function. 

But now someone could use the unsafe `triplicateImpl`. There is a trivial solution for thar problem: you don’t export the foreign function,  you only export your wrapper that takes the constraint.

```purescript
module ComputerTools
  ( Computer
  , compute
  , triplicate
  , Box(..)
  ) where

class Computer f where
  compute :: f -> Int

foreign import triplicateImpl :: forall a. (a -> Int) -> a -> Int

triplicate :: forall c. (Computer c) => c -> Int
triplicate = triplicateImpl compute

--

data Box = Box Int

instance computerBox :: Computer Box where
  compute (Box x) = x

```

```js
// module ComputerTools 
 
exports.triplicateImpl = function(shouldBeComputeFunction) { 
  return function(shouldBeComputerInstance) { 
    return 3*shouldBeComputeFunction(shouldBeComputerInstance); 
  }; 
}; 

```
