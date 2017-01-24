# Related Projects

PureScript might be compared to other AltJS projects such as Roy, Haste, Fay, Elm and GHCJS. Certainly, there is a lot of overlap in terms of syntax, but the goals of PureScript listed above separate it in one or more ways from each of these languages.

## Roy

Roy is probably the most similar language on the list, and was a large influence on the development of PureScript. There are however, key differences in the foreign function interface, the type system and the choice of development language (Haskell vs. Javascript)

## Haste, Fay, GHCJS

Projects such as Haste, Fay and GHCJS aim to compile Haskell (or a subset) to Javascript, while preserving Haskell's semantics.

This approach gives the advantage that tools and libraries can be shared with Haskell, but often at the cost of the size of the generated Javascript. This is the main practical difference between PureScript and these projects.

PureScript takes inspiration from Haskell in terms of its syntax and type system, but does not try to model its semantics. In this sense, PureScript can be thought of as simply an environment in which to write principled Javascript.

## Elm

Elm also shares a lot in terms of functionality with PureScript. Elm is designed for functional reactive programming, and focusses on tools and language features suitable for that domain, while PureScript focusses on the development of purely functional core application logic. Another difference between PureScript and Elm is PureScript’s lack of a runtime system.
