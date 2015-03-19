PSCi "PureScript Interactive" is the REPL for PureScript. It is often a good way to explore your code, do some basic testing, or to get a feel for a new library.

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

