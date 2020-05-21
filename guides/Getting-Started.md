# Getting Started with PureScript

Let's walk through the basics of getting set up to use the PureScript compiler `purs`, and its interactive mode `purs repl`.

We'll start with the installation of the compiler and Spago build tool, and then go through the basic usage of `purs repl`, working towards a solution of problem 1 from [Project Euler](http://projecteuler.net/problem=1).

#### Installing the Compiler

You'll need [Node.js and npm](https://docs.npmjs.com/getting-started/installing-node) and to be [able to install global packages](https://docs.npmjs.com/getting-started/fixing-npm-permissions#option-1-change-the-permission-to-npm-s-default-directory) to proceed.

The Purescript compiler (`purs`) can be installed with npm:

    npm install -g purescript

(It can also be installed from [Hackage](http://hackage.haskell.org/package/purescript), or by downloading the latest [binary bundle](https://github.com/purescript/purescript/releases) for your OS. If you do so, make sure the `purs` executable is on your `$PATH`.)

#### Setting up the Development Environment

[Spago](https://github.com/spacchetti/spago) is the recommended package manager and build tool for PureScript.

If you don't have Spago installed, install it now:

    npm install -g spago

Create a new project in an empty directory using `spago init`:

    spago init

Your directory should now contain the following files:

- `packages.dhall` - contains Spago configuration
- `spago.dhall` - contains library dependency information
- `src/Main.purs` - Entry point module for your project
- `test/Main.purs` - An empty test suite

At this point, you should be able to build the project and run the tests:

    spago build
    spago test

You should see output similar to the following:

    [info] Installation complete.
    [info] Build succeeded.
    🍝
    You should add some tests.
    [info] Tests succeeded.

If everything was built successfully, and the tests ran without problems, then the last line should state "Tests succeeded."

#### Installing Dependencies

Dependencies can be installed using Spago. We will be using the `purescript-lists` library shortly, so install it now:

    spago install lists

#### Working in PSCI

PSCi is the interactive mode of PureScript. It is useful for working with pure computations, and for testing ideas.

Open PSCi by typing `spago repl` at the command line. Optionally, you can create a file in your directory called `.purs-repl`, which contains instructions to PSCi to load your modules and dependencies. If you invoke the PSCi executable directly, you would need to load these files by hand.

    PSCi, version 0.12.0
    Type :? for help

    import Prelude

    >

As the introduction indicates, you can type `:?` to see a list of commands:

    The following commands are available:

    :?                        Show this help menu
    :quit                     Quit PSCi
    :reload                   Reload all imported modules while discarding bindings
    :clear                    Discard all imported modules and declared bindings
    :browse      <module>     See all functions in <module>
    :type        <expr>       Show the type of <expr>
    :kind        <type>       Show the kind of <type>
    :show        import       Show all imported modules
    :show        loaded       Show all loaded modules
    :paste       paste        Enter multiple lines, terminated by ^D
    :complete    <prefix>     Show completions for <prefix> as if pressing tab

    Further information is available on the PureScript documentation repository:
    --> https://github.com/purescript/documentation/blob/master/guides/PSCi.md

We will use a selection of these commands during this tutorial.

Start by pressing the Tab key to use the autocompletion feature. You will see a collection of names of functions from the Prelude which are available to use.

To see the type of one of these values, first import the appropriate module using the `import` command.

Next, use the `:type` command, followed by a space, followed by the name of the value:

    > import Prelude
    > :type map
    forall a b f. Functor f => (a -> b) -> f a -> f b

    > import Data.List
    > :type zip
    forall a b. List a -> List b -> List (Tuple a b)

We will be using some of the functions from the `Prelude` and `Data.List` modules, so make sure you have imported those by using the `import` keyword:

    import Prelude
    import Data.List

Note that using `Tab` to autocomplete names can be a useful time-saving device in `psci`.

#### Solving Project Euler #1

The following problem is taken from [Project Euler](http://projecteuler.net/problem=1):

> If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
>
> Find the sum of all the multiples of 3 or 5 below 1000.

We can solve this problem neatly using functions and function composition, directly in the REPL.

Let's start by listing all of the natural numbers below 1000 as a list. We can do this using the `range` function from `Data.List`:

    > range 0 999

You should see a list with 1000 elements printed to the command line.

This value can be given a name:

    > ns = range 0 999

Now let's filter out all of those elements which do not meet the criterion. We can use the `filter` function from `Data.List`, by providing a predicate function as its first argument:

    > multiples = filter (\n -> mod n 3 == 0 || mod n 5 == 0) ns

You can see the result by evaluating `multiples` if you like, or even check its type:

    > multiples
    (0 : 3 : 5 : 6 : ...
    > :type multiples
    List Int

Now we need to find the sum of the `multiples` list, to complete the solution. We can use the `sum` function from the `Data.Foldable` module.

    > import Data.Foldable
    > sum multiples
    233168

When you have finished using PSCi, type `:quit` to quit:

    > :quit
    See ya!

#### Compiling a Solution

Now that we've seen how to use the REPL to reach the answer, let's move our solution into a source file and compile it.

Create a new text file `src/Euler.purs` and copy the following code:

```purescript
module Euler where

import Prelude

import Data.List (range, filter)
import Data.Foldable (sum)

ns = range 0 999

multiples = filter (\n -> mod n 3 == 0 || mod n 5 == 0) ns

answer = sum multiples
```

It is possible to load this file directly into the REPL and to continue working:

    spago repl
    > import Euler
    > answer
    233168
    > :quit
    See ya!

Alternatively, we can use Spago to compile our new module to JavaScript:

    spago build

This will compile each module present in `src/` into a separate file in the `output/` directory.

The compiler will display several warnings about missing type declarations. In general it is considered good practice to provide explicit type signatures. In this guide, they are left out for brevity. In the absence of type signatures, the PureScript compiler infers types automatically but will remind us to consider adding them.

#### Writing a Test Suite

To test our code, we'll use the `purescript-assert` library:

    spago install assert

Modify the `test/Main.purs` file, and add the following code:

```purescript
module Test.Main where

import Prelude

import Euler (answer)
import Test.Assert (assert)

main = do
  assert (answer == 233168)
```

Our "test suite" is just a single assertion that the `answer` value equals the correct integer. In a real test suite, we might use the `Effect` monad to compose multiple tests in our `main` function.

Run the tests using `spago test`, and you should hopefully see "Tests OK" in the last line.

#### Creating Executables

We can modify the `main` function in the `src/Main.purs` module to print our result to the console:

```purescript
module Main where

import Prelude

import Euler (answer)
import Effect.Console (log)

main = do
  log ("The answer is " <> show answer)
```

The `spago run` command can be used to compile and run the `Main` module:

    > spago run
    [info] Build succeeded.
    The answer is 233168

#### What Next?

If you're new to typed functional programming, your next stop should be [PureScript by Example](https://book.purescript.org/), which will walk you through learning PureScript by solving practical problems.

If you are already familiar with an ML-family language, like Haskell or Elm, PureScript by Example should still be appropriate as a starting point, but you may alternatively want to start by browsing the [language reference in the documentation repository](https://github.com/purescript/documentation/tree/master/language) instead. The language reference gives a more brief, reference-style description of the language, and is aimed at those who are already somewhat familiar with typed functional programming. There is also a [Differences from Haskell](https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md) page which Haskell programmers will find useful.

New PureScript programmers are also encouraged to spend some time browsing [Pursuit](https://pursuit.purescript.org), which hosts generated API documentation for PureScript libraries. In particular it is worth familiarising yourself with the [core libraries](https://github.com/purescript) (i.e., those which are hosted under the `purescript` organisation on GitHub), and especially the [prelude](https://pursuit.purescript.org/packages/purescript-prelude), as these provide many basic concepts which are frequently useful for writing programs.
