If you want to avoid curried functions in performance-sensitive parts of your code, the compiler has special optimizations built in for all the `mkFn*` and `runFn*` functions defined in [Data.Function](https://pursuit.purescript.org/packages/purescript-functions/0.1.0/docs/Data.Function) ([purescript-functions](https://github.com/purescript/purescript-functions))

For example, the PureScript optimizer  will turn this:

```purescript
addUncurried = mkFn3 $ \x y z -> x + y + z;
```

into

```js
var addUncurried = function(x, y, z) {
    return x + y + z;
};
```

and this:

```purescript
runFn3 addUncurried 1 2 3
```

into

```js
addUncurried(1, 2, 3);
```

