The Foreign Function Interface allows you to interact with code written in JavaScript (or whichever backend you're using).

Importing Values
----------------

The `foreign import` keywords declare a value which is defined in JavaScript, and its type:

```purescript
-- src/Math.purs
module Math where
foreign import pow :: Number -> Number -> Number
```

When importing values from the FFI, the values themselves are defined in separate files. The filename should be the same as the PureScript source file name, except with the ".purs" replaced with a file extension depending on the backend. For example, if the above code had been written in `src/Math.purs`, then the corresponding FFI file would be at `src/Math.js`, and it might look like this:

```javascript
// src/Math.js
"use strict";
exports.pow = function(x) {
  return function(y) {
    return Math.pow(x,y);
  };
};
```

The compiler will check that your FFI file does export any values that you have imported via `foreign import`. However, the compiler cannot check that values defined in the FFI have the correct runtime representation based on the type they are given: it is your responsibility to ensure that, if you have declared that a value you imported is an `Int`, this value is actually an `Int` (and not an array, or function, or anything else).

Importing Types
---------------

To declare a new abstract type (with no constructors), use `foreign import data` and provide the kind:

```purescript
foreign import data DOMElement :: Type

foreign import document :: {
  createElement :: String -> DOMElement
}
```

When declaring types in this way, you may declare your type to have any kind, not just `Type`. For example, to declare a row of symbols:

```purescript
foreign import data MyRow :: Row Symbol
```
