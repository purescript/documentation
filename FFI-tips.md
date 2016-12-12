## Use `mkFn` and `runFn`

All functions in PureScript take exactly one argument, but when writing an interface to an existing JavaScript library often there is a need to expose multiple argument functions. One way to deal with this is to write some inline FFI code that "manually" curries a version of the function you want to bring in to PureScript:

```purescript
module Path where

foreign import joinPath :: FilePath -> FilePath -> FilePath
```

```javascript
// module Path

exports.joinPath = function(start) {
  return function(end) {
    return require('path').join(start, end);
  };
};
```

This is quite tedious and error prone, so there's an alternative representation of function types, `Fn0` up to `Fn10` available from the module `Data.Function` (from [`purescript-functions`](https://github.com/purescript/purescript-functions)). Making use of these types allows us to greatly simplify the previous example:

```purescript
module Path where

foreign import joinPathImpl :: Fn2 FilePath FilePath FilePath
```

```javascript
// module Path

exports.joinPathImpl = require('path').join;
```

However, these `Fn0`..`Fn10` types cannot be applied as normal PureScript functions, the require a corresponding `runFn0`..`runFn10` call to execute. The `runFn` definitions essentially do the work of taking a multi-argument function and returning a curried version for you.

Taking the previous example again, to avoid having to use `runFn2` every time we want to make use of `joinPath`, the usual way of doing this would be to suffix the foreign import with "Impl" and then define a more PureScript-friendly version that uses `runFn2`:

```purescript
foreign import joinPathImpl :: Fn2 FilePath FilePath FilePath

joinPath :: FilePath -> FilePath -> FilePath
joinPath = runFn2 joinPathImpl
```

The module would then hide the `joinPathImpl` export, only revealing our nice `joinPath` version.

Special support has been added to the compiler as of PureScript 0.5.4 to inline `runFn` calls when they are fully saturated (that is, applied with all the arguments at once), so it is recommended to avoid point-free style when making definitions that use `runFn`. Taking the above example again:

```purescript
joinPath :: FilePath -> FilePath -> FilePath
joinPath start end = runFn2 joinPathImpl start end
```

## Avoid directly calling PS code from foreign modules

When implementing things in the FFI, sometimes it's useful to be able to call other functions or make use of data constructors defined in PureScript. For example, if you wanted to write a function that returned a `Maybe` you might do something like this:

```purescript
foreign import doSomethingImpl :: forall a. Fn2 (a -> Boolean) a (Maybe a)

doSomething :: forall a. (a -> Boolean) -> a -> Maybe a
doSomething fn x = runFn2 doSomethingImpl fn x
```

```javascript
exports.doSomethingImpl = function(fn, x) {
  if (fn(x)) {
    return Data_Maybe.Just.create(x);
  } else {
    return Data_Maybe.Nothing.value;
  }
};
```

Calling these functions directly in the FFI code isn't recommended as it makes the code brittle to changes in the code generator. Additionally, doing this can cause problems when using `psc-bundle` for dead code elimination.

The recommended approach is to add extra arguments to your FFI-defined function to accept the functions you need to call as arguments:

```purescript
foreign import doSomethingImpl :: forall a. Fn4 (a -> Maybe a) (Maybe a) (a -> Boolean) a (Maybe a)

doSomething :: forall a. (a -> Boolean) -> a -> Maybe a
doSomething fn x = runFn4 doSomethingImpl Just Nothing fn x
```

```javascript
exports.doSomethingImpl = function(just, nothing, fn, value) {
  if (fn(value)) {
    return just(value);
  } else {
    return nothing;
  }
};
```

This way the compiler knows `Just` and `Nothing` are used so you don't need to worry about dead code elimination removing them, and also you don't have to deal with any future changes that may happen to the way code is generated for data constructors in the generated output.

This technique also helps when you want to call a function that is a type class member via the FFI. A contrived example using `show`:

```purescript
foreign import showSomethingImpl :: forall a. Fn3 (Maybe a -> Boolean) (a -> String) (Maybe a) String

showSomething :: forall a. (Show a) => Maybe a -> String
showSomething x = runFn3 showSomethingImpl isJust show x
```

```javascript
exports.doSomethingImpl = function(isJust, show, value) {
  if (isJust(value)) {
    return "It's something: " + show(value);
  } else {
    return "It's nothing.";
  }
};
```

By moving the `show` reference out to `showSomething` the compiler will pick the right `Show` instance for us at that point, so we don't have to deal with typeclass dictionaries in `showSomethingImpl`.

## Why Doesn't my `Eff` Work When Passed to a Normal JS Function?
["Representing Side Effects"](https://leanpub.com/purescript/read#leanpub-auto-representing-side-effects) in *PureScript by Example*.

In order to avoid prematurely evaluating effects (or evaluating effects that should not be evaluated at all), PureScript wraps them in constant functions:
```javascript
exports.myEff = function() {
  return doSomethingEffectful(1, 2, 3);
}
```
which is imported to PureScript as:
```purescript
foreign import myEff :: forall eff. Eff (myEff :: MYEFF | eff) SomeType
```

## FFI Libraries
There are a number of libraries for easing the process or writing FFI code. @paf31 wrote an [introduction](https://github.com/paf31/24-days-of-purescript-2014/blob/master/3.markdown) to these in his 2014 ["24 Days of PureScript"](https://gist.github.com/paf31/8e9177b20ee920480fbc). Note that it shows the now deprecated (as of PureScript 0.7.0) inline FFI.

**TODO** Performance and other considerations such as Browserify (`require`ing modules dynamically)

## TODO
- Avoiding Duplicate Labels
