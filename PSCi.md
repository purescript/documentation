PSCi "PureScript Interactive" is the REPL for PureScript. It is often a good way to explore your code, do some basic testing, or to get a feel for a new library.

## `purescript-psci-support`

```text
$ psci

PSCi requires the purescript-psci-support package to be installed.
You can install it using Bower as follows:

  bower i purescript-psci-support --save-dev

For help getting started, visit http://wiki.purescript.org/PSCi
```

The PureScript compiler suite (`psc`, `psci`, etc), unlike most compilers, does not ship with a standard library. In PureScript, even `Prelude` is a normal module, just like any other.

Consequentially, `psci` requires a specific library to be installed in order to be able to evauluate terms in the REPL `purescript-psci-support` defines the `Eval` type class for this purpose.

However, normally, you won't need to worry about this, because Pulp takes care of installing `purescript-psci-support` by default:

```text
$ npm install -g pulp # Install pulp

$ cd my-project # Enter an empty folder

$ pulp init # Initialize a pulp environment

$ pulp psci # Fire up the interpreter psci

PSCi, version 0.9.1
Type :? for help

> "hello"
"hello"

> import Prelude

> 1 + 2 * 3
7

> import Control.Monad.Eff.Console

> log "print this to the screen"
print this to the screen
unit
```

For more information about getting started with a development environment, see [this guide](http://www.purescript.org/learn/getting-started/).

## Basic usage

Type expressions into the REPL to have them evaluated:

    > 3 + 4
    7

Introduce bindings with `let`:

    > let x = 4
    > x + 9
    13

You can also define data types, type classes, and type class instances (you may need to use multi-line mode for this):

    > data Season = Spring | Summer | Autumn | Winter
    > class Shout a where shout :: a -> a
    > instance shoutString :: Shout String where shout s = s <> "!"

## PSCi Without Pulp

PSCi can be run directly, by specifying a list of PureScript source files as globs:

    psci 'src/**/*.purs' 'bower_components/purescript-*/src/**/*.purs'

