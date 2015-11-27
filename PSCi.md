PSCi "PureScript Interactive" is the REPL for PureScript. It is often a good way to explore your code, do some basic testing, or to get a feel for a new library.

## What's all this about `purescript-console`?

```
:? shows help
PSCi requires the purescript-console module to be installed.
For help getting started, visit http://wiki.purescript.org/PSCi
```

The PureScript compiler suite (`psc`, `psci`, etc), unlike most compilers, does not ship with a standard library. In PureScript, `Prelude` is a normal module, just like any other.

Consequentially, `psci` requires a specific library to be installed in order to be able to do things like printing results to the console - namely, `purescript-console`.

In addition, `psci` has no expected source file paths hardcoded in, and so it needs to be told where to load PureScript source files from. This can be done by writing a `.psci` file.

However, normally, you won't need to worry about these things. Most people use tools which take care of these things for you, such as `pulp`. For help getting started with a development environment, see [this guide](http://www.purescript.org/learn/getting-started/).

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
    > class (Monoid v) <= Measured a v where measure :: a -> v
    > instance measuredMeasured :: (Monoid a) => Measured a a where measure = id

## Loading and importing modules

You can load a file containing PureScript source code with the `:load` directive:

    > :load src/Data/Array.purs

Once a file has been loaded, values and types in the module(s) that it
contains are available fully qualified:

    > :type Data.Array.concat
    forall a. [[a]] -> [a]

Alternatively, you can import a loaded module in order to bring types and
values into the current scope - then, you can use them unqualified.

Modules can be imported normally, as in PureScript code, eg:

    > import Control.Monad
    > import Prelude.Unsafe (unsafeIndex)
    > import qualified Data.List as L

