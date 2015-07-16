Welcome to the PureScript wiki! This wiki is meant to be a collaborative effort, so please feel free to add/edit content where it is appropriate to do so.

## Introduction

PureScript is a small strongly, statically typed compile-to-JS language with a number of interesting features, such as:

- Type Inference
- Higher Kinded Polymorphism
- Support for basic Javascript types
- Extensible records
- Extensible effects
- Optimizer rules for generation of efficient Javascript
- Pattern matching
- Simple FFI
- Modules
- Rank N Types
- Do Notation
- Tail-call elimination
- Type Classes

## Libraries

- [Recommended Libraries](https://github.com/purescript/purescript/wiki/Recommended-Libraries)
- [Style Guide](https://github.com/purescript/purescript/wiki/Style-Guide)
- [24 Days of PureScript 2014](https://gist.github.com/paf31/8e9177b20ee920480fbc)

## Tools

- [Editor and tool support](https://github.com/purescript/purescript/wiki/Editor-and-tool-support)
- [Pursuit search engine](http://pursuit.purescript.org)

## Development Environments

- [PureScript and Docker](https://github.com/purescript/purescript/wiki/PureScript-and-Docker)
- [PureScript and NixOS](https://pr06lefs.wordpress.com/2015/01/11/get-started-with-purescript-on-nixos/)

## Articles

- [First Steps With PureScript - Solving Project Euler #1](http://www.purescript.org/learn/getting-started/)
- [The Foreign Function Interface](http://www.purescript.org/learn/ffi/)
- [Handling Native Effects with the Eff Monad](http://www.purescript.org/learn/eff/)
- [FFI Tips](https://github.com/purescript/purescript/wiki/FFI-tips)
- [Test your Javascript with QuickCheck](https://github.com/purescript/purescript/wiki/Test-your-Javascript-with-QuickCheck)

## Language Guides

The [PureScript Book](https://leanpub.com/purescript/read) is the recommended approach to learning the language, since it covers more material in greater depth. Below are quick references:

- [Intro](https://github.com/purescript/purescript/wiki/Language-Guide:-Intro)
- [Getting Started](https://github.com/purescript/purescript/wiki/Language-Guide:-Getting-Started)
- [Types](https://github.com/purescript/purescript/wiki/Language-Guide:-Types)
- [Kinds](https://github.com/purescript/purescript/wiki/Language-Guide:-Kinds)
- [Syntax](https://github.com/purescript/purescript/wiki/Language-Guide:-Syntax)
- [Type Classes](https://github.com/purescript/purescript/wiki/Language-Guide:-Type-Classes)
- [Pattern Matching](https://github.com/purescript/purescript/wiki/Language-Guide:-Pattern-Matching)
- [Modules](https://github.com/purescript/purescript/wiki/Language-Guide:-Modules)
- [FFI](https://github.com/purescript/purescript/wiki/Language-Guide:-FFI)
- [Records](https://github.com/purescript/purescript/wiki/Language-Guide:-Records)

## Related Languages

- [Related Projects](https://github.com/purescript/purescript/wiki/Related-Projects)
- [Differences from Haskell](https://github.com/purescript/purescript/wiki/Differences-from-Haskell)

## TL;DR
* [Install PureScript](http://www.purescript.org/download/)
* [Install Node.js](https://nodejs.org/): `brew install node`/`pacman -S nodejs`/`choco install nodejs`/[Download Node.js](https://nodejs.org/)
* [Install Pulp](https://github.com/bodil/pulp): `npm install -g pulp`
* GO: `pulp init`, `pulp dep install purescript-tuples`, `pulp build --to output.js`, etc.
