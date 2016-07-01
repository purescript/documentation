You have a bad FFI file; have a look at how to define them: http://www.purescript.org/learn/ffi/#foreign-modules

Each foreign module file should have a comment `// module X.Y.Z` where `X.Y.Z` is the name of the module it is associated with.

Values should be provided in the form `exports.foo = ...`, similar to a CommonJS module.

An example can be found [here](https://github.com/purescript/purescript-eff/blob/v0.1.0-rc.1/src/Control/Monad/Eff.js).
