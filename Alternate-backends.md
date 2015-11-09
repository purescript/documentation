There are various alternatives to `psc`'s default JavaScript backend:

| source code                                                                           | PS version | target        |       |                        | 
|:--------------------------------------------------------------------------------------|:-----------|:--------------|:------|:-----------------------|
| [andyarvanitis/pure11](https://github.com/andyarvanitis/pure11)                       | 0.7.5.3    | C++11         | active | all tests in examples/passing are passing |
| [andyarvanitis/pure14](https://github.com/andyarvanitis/pure14)                       | 0.7.4.1    | C++14         | TBD |  focus shifted to pure11 for now  |
| [slamdata/truffled-purescript](https://github.com/slamdata/truffled-purescript)       | 0.7.0-rc.1 | Truffle (JVM) | stale | translates CoreFN JSON |
| [osa1/psc-lua](https://github.com/osa1/psc-lua)                                       | 0.5.x      | Lua           | stale |                        |
| [Gabriel439/Purescript-to-Python](https://github.com/Gabriel439/Purescript-to-Python) |            | Python        | unusable atm |                        |
| [epost/psc-query](https://github.com/epost/psc-query)                                 | 0.7.3.0    | Datalog       | toy   |                        |