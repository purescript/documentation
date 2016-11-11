PSCi "PureScript Interactive" is the REPL for PureScript. It is often a good way to explore your code, do some basic testing, or to get a feel for a new library.

## Getting Started

Use Pulp to configure and start PSCi:

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

## `paste-mode` (previously `multi-line-mode`)

Enter `:paste` (or `-p`) to start with `multi-line` mode. Terminate it with `Control-D` key.

```
> import Prelude                                                                 
> :paste                                                                         
… let
…     add :: Int -> Int -> Int
…     add = \x y -> x + y      
> add 10 20
30                                             
```

[Demo](https://asciinema.org/a/0y56unmja6fqire01x20zb5xx)

## `purescript-psci-support`

```text
$ psci

PSCi requires the purescript-psci-support package to be installed.
You can install it using Bower as follows:

  bower i purescript-psci-support --save-dev

For help getting started, visit http://wiki.purescript.org/PSCi
```

The PureScript compiler suite (`psc`, `psci`, etc), unlike most compilers, does not ship with a standard library. In PureScript, even `Prelude` is a normal module, just like any other. Consequentially, `psci` requires a specific library to be installed in order to be able to evaluate terms in the REPL.

`purescript-psci-support` defines the `Eval` type class for this purpose. Instances of `Eval` are provided for `Show`able types, and for `Eff`, so that we can evaluate actions in the REPL. Library implementors might like to provide `Eval` instances for their own `Eff`-like types.

## PSCi Without Pulp

PSCi can be run directly, by specifying a list of PureScript source files as globs:

    psci 'src/**/*.purs' 'bower_components/purescript-*/src/**/*.purs'

