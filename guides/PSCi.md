PSCi "PureScript Interactive" is the REPL for PureScript. It is often a good way to explore your code, do some basic testing, or to get a feel for a new library.

## Getting Started

Use Spago to configure and start PSCi:

```text
$ npm install -g spago       # Install Spago

$ cd my-project              # Enter an empty folder

$ spago init                 # Initialize a Spago environment

$ spago repl                 # Fire up the interpreter psci

...
PSCi, version 0.13.6
Type :? for help

import Prelude

> "hello"
"hello"

> import Prelude

> 1 + 2 * 3
7

> import Effect.Console

> log "print this to the screen"
print this to the screen
unit
```

For more information about getting started with a development environment, see [this guide](http://www.purescript.org/learn/getting-started/).

## Basic usage

Type expressions into the REPL to have them evaluated:

    > 3 + 4
    7

Introduce bindings with `=`:

    > x = 4
    > x + 9
    13

You can also define data types, type classes, and type class instances (you may need to use multi-line mode for this):

    > data Season = Spring | Summer | Autumn | Winter
    > class Shout a where shout :: a -> a
    > instance shoutString :: Shout String where shout s = s <> "!"

## Paste mode (previously `--multi-line-mode`)

Enter `:paste` (or `:pa`) to enter multi-line (or "paste") mode. Terminate it with `Control-D` key.

```
> import Prelude
> :paste
… add :: Int -> Int -> Int
… add = \x y -> x + y
> add 10 20
> (^D)
30
```

[Demo](https://asciinema.org/a/0y56unmja6fqire01x20zb5xx)

## `purescript-psci-support`

```text
$ spago repl

PSCi requires the `purescript-psci-support` package to be installed.
You can install it using Spago by adding `"psci-support"` to the list of dependencies in `spago.dhall`.

For help getting started, visit http://wiki.purescript.org/PSCi
```

The PureScript compiler suite (i.e. the executable `purs`), unlike most compilers, does not ship with a standard library. In PureScript, even `Prelude` is a normal module, just like any other. Consequentially, `purs repl` requires a specific library to be installed in order to be able to evaluate terms in the REPL.

`purescript-psci-support` defines the `Eval` type class for this purpose. Instances of `Eval` are provided for `Show`able types, and for `Effect`, so that we can evaluate actions in the REPL.

## PSCi Without Spago

PSCi can be run directly, by specifying a list of PureScript source files as globs:

    purs repl 'src/**/*.purs' 'path/to/packages/**/*.purs'

This expects you to have downloaded the PureScript sources for your dependencies (include `psci-support`) under the path `path/to/packages`.

Note the single quotes—the purescript compiler itself knows how to expand globs (`*`) and recursive globs (`**`), single quotes prevent your shell from expanding them. (Bash for example doesn’t have recursive globbing enabled by default; don’t forget the single quotes.)
