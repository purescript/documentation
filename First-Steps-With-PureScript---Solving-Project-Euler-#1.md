By _Phil Freeman_

Welcome to the PureScript community blog! In this first post, I'm going to walk through the basics of getting set up to use the PureScript compiler `psc`, and its interactive mode `psci`.

I'll start with the installation of the compiler, go through the basic commands of `psc` and `psci`, working towards a solution of problem 1 from [Project Euler](http://projecteuler.net/problem=1).

### Installing the Compiler

PureScript can be installed from [Hackage](http://hackage.haskell.org/package/purescript). After installing the [Haskell Platform](http://www.haskell.org/platform), you can use the following to build the compiler from source:

    cabal update
    cabal install purescript

Make sure the `psc` executable is on your path. If not, it is usually installed to `~/.cabal/bin`.

### Setting up the Development Environment

PureScript's core libraries are configured to use the `grunt` build tool, and packages are available in the `bower` registry.

If you don't have `grunt` and `bower` installed, install them now:

    npm install -g grunt-cli bower

### Using the Starter Kit

This post will build upon the starter kit available [here](http://github.com/purescript/starter-kit). Start by cloning the project into a new directory:

    git clone git@github.com:purescript/starter-kit.git

Now install necessary packages using `npm`, and pull the necessary libraries from the `bower` registry:

    cd starter-kit
    npm install
    bower update

The starter kit contains the following files:

- `bower.json` - contains library dependency information.
- `package.json` - contains `npm` dependencies, including `grunt-purescript`
- `Gruntfile.js` - builds the project and its test suite using `grunt`.
- `.psci` - A configuration file for the interactive mode `psci`.

At this point, you should be able to build the project and run the tests:

    grunt

You should see output similar to the following:

    Running "clean:tests" (clean) task
    Cleaning tmp...OK

    Running "purescript:tests" (purescript) task
    >> Created file tmp/tests.js.

    :Running "execute:tests" (execute) task
    -> executing tmp/tests.js
    The differences of an empty list are empty.
    All tests passed
    The differences of a single-element list are empty.
    All tests passed
    The differences of a pair of equal elements are zero.
    All tests passed
    The diffs function returns Just (...) for a sorted list.
    All tests passed
    The diffs function returns Nothing for a reverse-sorted list with at least one pair of unequal elements.
    All tests passed
    -> completed tmp/tests.js (50ms)

    >> 1 file and 0 calls executed (60ms)

    Running "purescript-make:lib" (purescript-make) task
    >> Make was successful.

    Done, without errors.

If everything was built successfully, and the tests ran without problems, then the last line should state "Done, without errors."

### Working in PSCI

`psci` is the interactive mode of PureScript. It is useful for working with pure computations, and for testing ideas.

Open `psci` by typing `psci` at the command line.

     ____                 ____            _       _   
    |  _ \ _   _ _ __ ___/ ___|  ___ _ __(_)_ __ | |_ 
    | |_) | | | | '__/ _ \___ \ / __| '__| | '_ \| __|
    |  __/| |_| | | |  __/___) | (__| |  | | |_) | |_ 
    |_|    \__,_|_|  \___|____/ \___|_|  |_| .__/ \__|
                                           |_|        
    
    :? shows help

    Expressions are terminated using Ctrl+D
    >

As the introduction indicates, you can type `:?` to see a list of commands:

    The following commands are available:
    
        :?              Show this help menu
        :i <module>     Import <module> for use in PSCI
        :m <file>       Load <file> for importing
        :q              Quit PSCi
        :r              Reset
        :t <expr>       Show the type of <expr>

We will use a selection of these commands during this tutorial.

Start by pressing the Tab key to use the autocompletion feature. You will see a collection of names of functions from the Prelude which are available to use. The `.psci` configuration file in the project directory specifies a default set of modules to load on startup.

To see the type of one of these values, type the `:t` command, followed by a space, followed by the name of the value:

    > :t Data.Array.map
    forall a b. (a -> b) -> [a] -> [b]
    > :t Data.Tuple.curry
    forall a b c. (Data.Tuple.Tuple a b -> c) -> a -> b -> c

We will be using some of the functions from the `Data.Array` module, so import that module by using the `:i` command:

    :i Data.Array

Note that using `Tab` to autocomplete names can be a useful time-saving device in `psci`.

### Solving Project Euler #1

The following problem is taken from [Project Euler](http://projecteuler.net/problem=1):
 
> If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
>
> Find the sum of all the multiples of 3 or 5 below 1000.

We can solve this problem neatly using functions and function composition, directly in `psci`.

Let's start by listing all of the natural numbers below 1000 as an array. We can do this using the `range` function from `Data.Array`:

    > range 0 999
    [0,1,2,3,4,...

You should see an array with 1000 elements printed to the command line.

This value can be given a name, using a `let` binding:

    > let ns = range 0 999

Now let's filter out all of those elements which do not meet the criterion. We can use the `filter` function from `Data.Array`, by providing a predicate function as its first argument:

    > let multiples = filter (\n -> n % 3 == 0 || n % 5 == 0) ns

You can see the result by evaluating `multiples` if you like, or even check its type:

    > multiples
    [0,3,5,6,9,10,12,15,...
    > :t multiples
    [Prim.Number]

Now we need to find the sum of the `multiples` array, to complete the solution. We can use the `sum` function from the `Data.Foldable` module.

    > :i Data.Foldable
    > sum multiples
    233168

When you have finished using `psci`, type `:q` to quit:

    > :q
    See ya!

### Compiling a Solution

Now that we've seen how to use `psci` to reach the answer, let's move our solution into a source file, and compile it using `psc`.

Create a new text file `src/Euler.purs` and copy the following code:

``` haskell
module Euler1 where

import Data.Array
import qualified Data.Foldable as F

ns = range 0 999

multiples = filter (\n -> n % 3 == 0 || n % 5 == 0) ns

answer = F.sum multiples
```

It is possible to load this file directly into `psci` and to continue working:

    psci Euler1.purs
    > Euler1.answer
    233168
    > :q
    See ya!

Alternatively, we can use `grunt` to compile the `Euler1.purs` file to Javascript:

    grunt

This will compile each module present in `src/` into a separate file under `js/`.

### Conclusion

That's all for this post. We've seen how to use enough of the basics of `psc` and `psci` to compile and execute simple PureScript programs. If you would like more information, the [PureScript documentation](http://docs.purescript.org) lists all of the options for both `psc` and `psci`.

Until next time\...