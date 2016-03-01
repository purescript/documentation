### PureScript code

```
class Computer f where
  compute :: f -> Int

data Box = Box Int

instance computerBox :: Computer Box where
  compute (Box x) = x + 1

foreign import triplicate :: forall c. (Computer c) => c -> Int
```

### Foreign code
```
exports.triplicate = function(aComputerDictionary) {
  return function(aComputerInstance) {
    return 3*aComputerDictionary.compute(aComputerInstance);
  };
};
```
