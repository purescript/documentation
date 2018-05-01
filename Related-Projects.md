# Related Projects

PureScript might be compared to other AltJS projects such as Roy, Haste, Fay, Elm and GHCJS. Certainly, there is a lot of overlap in terms of syntax, but the goals of PureScript listed above separate it in one or more ways from each of these languages.

## Roy

Roy is probably the most similar language on the list, and was a large influence on the development of PureScript. There are however, key differences in the foreign function interface, the type system and the choice of development language (Haskell vs. Javascript)

## Haste, Fay, GHCJS

Projects such as Haste, Fay and GHCJS aim to compile Haskell (or a subset) to Javascript, while preserving Haskell's semantics.

This approach gives the advantage that tools and libraries can be shared with Haskell, but often at the cost of the size of the generated Javascript. This is the main practical difference between PureScript and these projects.

PureScript takes inspiration from Haskell in terms of its syntax and type system, but does not try to model its semantics. In this sense, PureScript can be thought of as simply an environment in which to write principled Javascript.

## Elm

Elm shares a lot in terms of functionality with PureScript. Elm is designed for web frontends with a simpler type system and a built-in Elm Architecture. Purescript aim to be general programming language targeting Javascript with a rich type system and also allows for more direct FFI usage.

In the Purescript ecosystem, the Elm Architecture is available through [Pux](http://purescript-pux.org/), a Elm 0.16-like library for web frontends. A truly component-based approach written purely in Purescript is available through [Halogen](https://github.com/slamdata/purescript-halogen) for web frontends which provides a similar interface contained within each component.
