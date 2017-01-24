Welcome to the PureScript wiki! This wiki is meant to be a collaborative effort, so please feel free to add/edit content where it is appropriate to do so.

## Introduction

PureScript is a small strongly, statically typed compile-to-JS language with a number of interesting features, such as:

- Type Inference
- Higher Kinded Polymorphism
- Support for basic JavaScript types
- Extensible records
- Extensible effects
- Optimizer rules for generation of efficient JavaScript
- Pattern matching
- Simple FFI
- Modules
- Rank N Types
- Do Notation
- Tail-call elimination
- Type Classes
- Functional Dependencies

## Libraries

- [Maintained Packages](ecosystem/Maintained-Packages.md)
- [Style Guide](guides/Style-Guide.md)
- [Related Projects](language/Related-Projects.md)

## Tools

- [Editor and tool support](ecosystem/Editor-and-tool-support.md)
- [Pursuit search engine](http://pursuit.purescript.org)

## Development Environments

- [PureScript and NixOS](https://pr06lefs.wordpress.com/2015/01/11/get-started-with-purescript-on-nixos/)

## Articles

- [First Steps With PureScript - Solving Project Euler #1](http://www.purescript.org/learn/getting-started/)
- [The Foreign Function Interface](guides/FFI.md)
- [FFI Tips](guides/FFI-Tips.md)
- [Generic Programming](guides/Generic.md)
- [Handling Native Effects with the Eff Monad](guides/Eff.md)
- [Test your JavaScript with QuickCheck](http://www.purescript.org/learn/quickcheck/)
- [PSCi](guides/PSCi.md)
- [Custom Type Errors](guides/Custom-Type-Errors.md)
- [PureScript Without Node](guides/PureScript-Without-Node.md)
- [24 Days of PureScript 2016](https://github.com/paf31/24-days-of-purescript-2016)

## Talks/Meetups

- [PureScript Presentations](ecosystem/PureScript-Presentations.md)
- [PureScript Meetups](ecosystem/PureScript-Meetups.md)

## Language Guides

The [PureScript Book](https://leanpub.com/purescript/read) is the recommended approach to learning the language, since it covers more material in greater depth. There is also a language guide, which is more useful as a reference:

- [Language Reference](language/README.md)

## Related Languages

- [Related Projects](Related-Projects.md)
- [Differences from Haskell](language/Differences-from-Haskell.md)
- [Alternate compiler backends](ecosystem/Alternate-backends.md) for various target languages

## TL;DR
* [Install PureScript](http://www.purescript.org/download/)
* [Install Node.js](https://nodejs.org/): `brew install node`/`pacman -S nodejs`/`choco install nodejs`/[Download Node.js](https://nodejs.org/)
* [Install Pulp](https://github.com/bodil/pulp): `npm install -g pulp bower`
* GO: `pulp init`, `bower install --save purescript-tuples`, `pulp build --to output.js`, etc.
