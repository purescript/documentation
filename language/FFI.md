# FFI

Importing Values
----------------

The ``foreign import`` keywords declare a value which is defined in Javascript, and its type:

```purescript
foreign import pow :: Number -> Number -> Number
```

Importing Types
---------------

To declare a new abstract type (with no constructors), use ``foreign import data`` and provide the kind::

```purescript
foreign import data DOM :: *
  	
foreign import document :: { 
  createElement :: String -> DOM  
}
```
