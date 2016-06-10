This error means that a foreign module could not be found. Usually, this means that you declared a foreign import in the PureScript module but didn't create a corresponding FFI module yet.

If a module uses any foreign imports, like this:

```purescript
foreign import myFunction :: Foo -> Bar
```

Then the PureScript compiler will expect to find an FFI module by taking the source file path, and replacing `.purs` with `.js`. So if you have `src/Foo/Bar.purs`, `psc` will look for the foreign module at `src/Foo/Bar.js`.