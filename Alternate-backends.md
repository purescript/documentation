There are various alternatives to `psc`'s default JavaScript backend:

| source code                                                                           | PS version | target        | status   | comments               | 
|:--------------------------------------------------------------------------------------|:-----------|:--------------|:------|:-----------------------|
| [pure11/pure11](https://github.com/pure11/pure11)  [(readme.md)](https://github.com/andyarvanitis/pure11/blob/master/README.md)            | 0.10.1    | C++11         | active | all tests in `./examples/passing` are passing |
| [lua-purescript/purescript](https://github.com/lua-purescript/purescript) | 0.9.1.0 | Lua | experimental | Very new and probably full of bugs. Largely untested |
| [andyarvanitis/purescript-clojure](https://github.com/andyarvanitis/purescript-clojure) |          | Clojure (JVM) | experimental |                 |
| [slamdata/truffled-purescript](https://github.com/slamdata/truffled-purescript)       | 0.7.5.x    | Truffle (JVM) | stale | translates CoreFN JSON |
| [osa1/psc-lua](https://github.com/osa1/psc-lua)                                       | 0.5.x      | Lua           | stale |                        |
| [PyreScript](https://github.com/joneshf/pyrescript)                                   | 0.9.1      | Python        | alpha |                        |
| [Gabriel439/Purescript-to-Python](https://github.com/Gabriel439/Purescript-to-Python) |            | Python        | unusable atm |                        |
| [epost/psc-query](https://github.com/epost/psc-query)                                 | 0.7.3.0    | Datalog       | toy   |                        |
| [purerl/purescript](https://github.com/purerl/purescript) | 0.10.1 | Erlang | experimental |