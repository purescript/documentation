## Use `mkFn` and `runFn`

All functions in PureScript take exactly one argument, but when writing an interface to an existing JavaScript library often there is a need to expose multiple argument functions. One way to deal with this is to write some inline FFI code that "manually" curries a version of the function you want to bring in to PureScript:

``` haskell
foreign import joinPath
  "function joinPath(start) {
  \  return function(end) {\
  \    return require('path').join(start, end);
  \  };\
  \}" :: FilePath -> FilePath -> FilePath
```

This is quite tedious and error prone, so there's an alternative representation of function types, `Fn0` up to `Fn10` available from the module `Data.Function` (included with the compiler). Making use of these types allows us to greatly simplify the previous example:

``` haskell
foreign import joinPath
  "var joinPath = require('path').join;" :: Fn2 FilePath FilePath FilePath
```

However, these `Fn0`..`Fn10` types cannot be applied as normal PureScript functions, the require a corresponding `runFn0`..`runFn10` call to execute. The `runFn` definitions essentially do the work of taking a multi-argument function and returning a curried version for you.

Taking the previous example again, to avoid having to use `runFn2` every time we want to make use of `joinPath`, the usual way of doing this would be to suffix the foreign import with "Impl" and then define a more PureScript-friendly version that uses `runFn2`:

``` haskell
foreign import joinPathImpl
  "var joinPathImpl = require('path').join;" :: Fn2 FilePath FilePath FilePath

joinPath :: FilePath -> FilePath -> FilePath
joinPath = runFn2 joinPathImpl
```

The module would then hide the `joinPathImpl` export, only revealing our nice `joinPath` version.

Special support has been added to the compiler as of PureScript 0.5.4 to inline `runFn` calls when they are fully saturated (that is, applied with all the arguments at once), so it is recommended to avoid pointfree style when making definitions that use `runFn`. Taking the above example again:

``` haskell
joinPath :: FilePath -> FilePath -> FilePath
joinPath start end = runFn2 joinPathImpl start end
```

## Avoid calling PS code from within JS directly

When implementing things in the FFI, sometimes it's useful to be able to call other functions or make use of data constructors defined in PureScript. For example, if you wanted to write a function that returned a `Maybe` you might do something like this:

``` haskell
foreign import doSomethingImpl
  "function doSomethingImpl(fn, x) {
  \  if (fn(x)) {\
  \    return Data_Maybe.Just.create(x);\
  \  } else {\
  \    return Data_Maybe.Nothing.value;\
  \  }\
  \}" :: forall a. Fn2 (a -> Boolean) a (Maybe a)

doSomething :: forall a. (a -> Boolean) -> a -> Maybe a
doSomething fn x = runFn2 doSomethingImpl fn x
```

Calling these functions directly in the FFI code isn't recommended as it makes the code brittle to changes in the code generator. Additionally, doing this can cause problems when using the `--module` compiler argument, as dead code elimination may remove the functions you're calling - the compiler doesn't attempt to read inline JavaScript FFI definitions.

A technique for working around this is to add extra arguments to your FFI-defined function to accept the functions you need to call as arguments:

``` haskell
foreign import doSomethingImpl
  "function doSomethingImpl(just, nothing, fn, value) {
  \  if (fn(value)) {\
  \    return just(value);\
  \  } else {\
  \    return nothing;\
  \  }\
  \}" :: forall a. Fn4 (a -> Maybe a) (Maybe a) (a -> Boolean) a (Maybe a)

doSomething :: forall a. (a -> Boolean) -> a -> Maybe a
doSomething fn x = runFn2 doSomethingImpl Just Nothing fn x
```

This way the compiler knows `Just` and `Nothing` are used so you don't need to worry about dead code elimination swiping them away, and also you don't have to deal with any future changes that may happen to the way code is generated for data constructors in the generated output.

(((also useful for avoiding typeclass constraints)))

## Making a normal JavaScript function Eff-typed

(((mkEff \_ -> ..., extra function FFI wrapping)))

## Why doesn't my Eff work when passed to a normal JS function?

(((Event listeners not calling the "extra" () etc)))

## Avoiding Duplicate Labels
