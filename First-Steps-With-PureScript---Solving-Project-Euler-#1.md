By _Phil Freeman_

Welcome to the PureScript community blog! In this first post, I'm going to walk through the basics of getting set up to use the PureScript compiler `psc`, and its interactive mode `psci`.

I'll start with the installation of the compiler, go through the basic commands of `psc` and `psci`, working towards a solution of problem 1 from [Project Euler](http://projecteuler.net/problem=1).

### Installing the Compiler

PureScript can be installed from [Hackage](http://hackage.haskell.org/package/purescript) or by downloading the latest [binary bundle](https://github.com/purescript/purescript/releases) for your OS.

Make sure the `psc` executable is on your path.

### Setting up the Development Environment

PureScript's core libraries are configured to use the Pulp build tool, and packages are available in the Bower registry.

If you don't have Pulp installed, install it now:

    npm install -g pulp

Create a new project in an empty directory using `pulp init`:

    pulp init

Your directory should now contain the following files:

- `bower.json` - contains library dependency information
- `bower_components/` - a directory for installed dependencies
- `src/Main.purs` - Entry point module for your project
- `test/Main.purs` - An empty test suite

At this point, you should be able to build the project and run the tests:

    pulp build
    pulp test

You should see output similar to the following:

    * Building project in /Users/paf31/Documents/Code/purescript/pulp-test
    * Build successful. Running tests...
    You should add some tests.
    * Tests OK.

If everything was built successfully, and the tests ran without problems, then the last line should state "Tests OK".

### Installing Dependencies

Dependencies can be installed using Bower, if you have it installed globally:

    bower i purescript-lists --save

If you want to use Pulp, you can run `pulp dep`:

    pulp dep i purescript-lists --save

### Working in PSCI

PSCi is the interactive mode of PureScript. It is useful for working with pure computations, and for testing ideas.

Open PSCi by typing `pulp psci` at the command line. Pulp will create a file in your directory called `.psci`, which contains instructions to PSCi to load your modules and dependencies. If you invoke the PSCi executable directly, you would need to load these files by hand.

     ____                 ____            _       _   
    |  _ \ _   _ _ __ ___/ ___|  ___ _ __(_)_ __ | |_ 
    | |_) | | | | '__/ _ \___ \ / __| '__| | '_ \| __|
    |  __/| |_| | | |  __/___) | (__| |  | | |_) | |_ 
    |_|    \__,_|_|  \___|____/ \___|_|  |_| .__/ \__|
                                           |_|        
    
    :? shows help
    >

As the introduction indicates, you can type `:?` to see a list of commands:

    The following commands are available:

    :?                        Show this help menu
    :quit                     Quit PSCi
    :reset                    Discard all imported modules and declared bindings
    :browse      <module>     See all functions in <module>
    :load        <file>       Load <file> for importing
    :foreign     <file>       Load foreign module <file>
    :type        <expr>       Show the type of <expr>
    :kind        <type>       Show the kind of <type>
    :show        import       Show all imported modules
    :show        loaded       Show all loaded modules

    Further information is available on the PureScript wiki:
    --> https://github.com/purescript/purescript/wiki/psci

We will use a selection of these commands during this tutorial.

Start by pressing the Tab key to use the autocompletion feature. You will see a collection of names of functions from the Prelude which are available to use.

To see the type of one of these values, use the `:type` command, followed by a space, followed by the name of the value:

    > :type Prelude.map
    forall a b f. (Prelude.Functor f) => (a -> b) -> f a -> f b
    > :type Data.List.zip
    forall a b. Data.List.List a -> Data.List.List b -> Data.List.List (Data.Tuple.Tuple a b)

We will be using some of the functions from the `Prelude` and `Data.List` modules, so import those by using the `import` keyword:

    import Prelude
    import Data.List

Note that using `Tab` to autocomplete names can be a useful time-saving device in `psci`.

### Solving Project Euler #1

The following problem is taken from [Project Euler](http://projecteuler.net/problem=1):
 
> If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
>
> Find the sum of all the multiples of 3 or 5 below 1000.

We can solve this problem neatly using functions and function composition, directly in `psci`.

Let's start by listing all of the natural numbers below 1000 as a list. We can do this using the `range` function from `Data.List`:

    > range 0 999

You should see a list with 1000 elements printed to the command line.

This value can be given a name, using a `let` binding:

    > let ns = range 0 999

Now let's filter out all of those elements which do not meet the criterion. We can use the `filter` function from `Data.List`, by providing a predicate function as its first argument:

    > let multiples = filter (\n -> mod n 3 == 0 || mod n 5 == 0) ns

You can see the result by evaluating `multiples` if you like, or even check its type:

    > multiples
    Cons 0 (Cons 3 (Cons 5 (Cons 6 (Cons ...
    > :type multiples
    List Int

Now we need to find the sum of the `multiples` array, to complete the solution. We can use the `sum` function from the `Data.Foldable` module.

    > import Data.Foldable
    > sum multiples
    233168

When you have finished using PSCi, type `:quit` to quit:

    > :quit
    See ya!

### Compiling a Solution

Now that we've seen how to use `psci` to reach the answer, let's move our solution into a source file, and compile it using `psc`.

Create a new text file `src/Euler.purs` and copy the following code:

```purescript
module Euler1 where

import Prelude

import Data.List (range, filter)
import Data.Foldable (sum)

ns = range 0 999

multiples = filter (\n -> mod n 3 == 0 || mod n 5 == 0) ns

answer = sum multiples
```

It is possible to load this file directly into PSCi and to continue working:

    pulp psci
    > Euler1.answer
    233168
    > :quit
    See ya!

Alternatively, we can use Pulp to compile our new module to Javascript:

    pulp build

This will compile each module present in `src/` into a separate file in the `output/` directory.

### Writing a Test Suite

To test our code, we'll use the `purescript-assert` library:

    bower i purescript-assert --save

Modify the `test/Main.purs` file, and add the following code:

```purescript
module Test.Main where

import Prelude
import Euler1 (answer)
import Test.Assert (assert)

main = do
  assert (answer == 233168)
```

Our "test suite" is just a single assertion that the `answer` value equals the correct integer. In a real test suite, we might use the `Eff` monad to compose multiple tests in our `main` function.

Run the tests using `pulp test`, and you should hopefully see "Tests OK" in the last line.

### Creating Executables

We can modify the `main` function in the `src/Main.purs` module to print our result to the console:

```purescript
module Main where

import Prelude
import Euler1
import Control.Monad.Eff.Console

main = do
  log ("The answer is " ++ show answer)
```

The `pulp run` command can be used to compile and run the `Main` module:

    > pulp run
    * Building project in /Users/paf31/Documents/Code/purescript/pulp-test
    * Build successful.
    The answer is 233168

### Conclusion

That's all for this post. We've seen how to use enough of the basics of `psc` and PSCi to compile, execute and test simple PureScript programs. If you would like more information, the [PureScript documentation](http://docs.purescript.org) lists all of the options for both `psc` and `psci`.

Until next time\...