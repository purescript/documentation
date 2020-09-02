# `DeclConflict` Error

## Example
Each of these pairs will fail with a DeclConflict error.
```purescript
module Main where

data T = Fail | Fail

data T1 = Fail1
data T2 = Fail1

class Fail2
data T3 = Fail2
```

## Cause

This error occurs when a data constructor, type, or type class conflicts with another data constructor, type, or type class.

## Fix
This can be fixed by using non-conflicting names or by putting conflicting names into a separate module. Then, if the names are later used together in a module they can be distinguished via a qualified import:
```purescript
module Main where

import Module1 as Module1 -- includes: data Works = Works
import Module2 as Module2 -- includes: data Works = Works

x :: Module1.Works
x = Module1.Works

y :: Module2.Works
y = Module2.Works 
```

