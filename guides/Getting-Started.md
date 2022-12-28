# Getting Started with PureScript

Let's walk through the basics of getting set up to use the PureScript compiler `purs`, and its interactive mode `purs repl`.

We'll start with the installation of the compiler and Spago build tool, and then go through the basic usage of `purs repl`, working towards a solution of problem 1 from [Project Euler](http://projecteuler.net/problem=1).

## Installing the Compiler

You'll need to install [Node.js and npm](https://docs.npmjs.com/getting-started/installing-node).  We recommend installing [Node.js and npm via a node version manager](https://docs.npmjs.com/getting-started/installing-node) to avoid issues with installing packages globally. If you choose to install it manually, you might experience the [`EACCES` error when installing packages globally](https://docs.npmjs.com/getting-started/fixing-npm-permissions#option-1-change-the-permission-to-npm-s-default-directory).

Install the PureScript compiler (`purs`) with npm:

    npm install -g purescript

Try running the PureScript compiler on the command line to verify that the PureScript compiler executables are available on your `$PATH`:

    purs

It can also be installed from [Hackage](http://hackage.haskell.org/package/purescript), or by downloading the latest [binary bundle](https://github.com/purescript/purescript/releases) for your OS. If you do so, make sure the `purs` executable is on your `$PATH`.

## Setting up the Development Environment

[Spago](https://github.com/spacchetti/spago) is the recommended package manager and build tool for PureScript.

If you don't have Spago installed, install it now:

    npm install -g spago

Create a new project in an empty directory using `spago init`:

    mkdir my-project
    cd my-project
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

## Installing Dependencies

Dependencies can be installed using Spago. We will be using the `lists` library shortly, so install it now:

    spago install lists

The `lists` library sources should now be available in the `.spago/lists/{version}/` subdirectory, and will be included when you compile your project.

## Working in PSCI

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
    :show        print        Show the repl's current printing function
    :paste       paste        Enter multiple lines, terminated by ^D
    :complete    <prefix>     Show completions for <prefix> as if pressing tab
    :print       <fn>         Set the repl's printing function to <fn> (which must be fully qualified)

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

## Solving Project Euler #1

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

## Compiling a Solution

Now that we've seen how to use the REPL to reach the answer, let's move our solution into a source file and compile it.

Create a new text file `src/Euler.purs` and copy the following code:

    module Euler where

    import Prelude

    import Data.List (range, filter)
    import Data.Foldable (sum)

    ns = range 0 999

    multiples = filter (\n -> mod n 3 == 0 || mod n 5 == 0) ns

    answer = sum multiples

This sample illustrates a few key ideas regarding modules:

- Every file begins with a module header. A module name consists of one or more capitalized words separated by dots. In this case, only a single word is used, but `My.First.Module` would be an equally valid module name.
- Modules are imported using their full names, including dots to separate the parts of the module name. Here, we import the `Prelude` module, which provides `mod`, `==`, and many other common functions. We also import `Data.List` which provides the explicitly-listed `range` and `filter` functions. We can either import all functions in a module implicitly, as is done with `Prelude`, or list them explicitly. Guidelines are to only have one module with implicit imports.

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

## Writing a Test Suite

To test our code, we'll use the `assert` library:

    spago install assert

Replace the contents of the `test/Main.purs` file with the following code:

    module Test.Main where

    import Prelude

    import Euler (answer)
    import Test.Assert (assert)

    main = do
    assert (answer == 233168)

Our "test suite" is just a single assertion that the `answer` value equals the correct integer. In a real test suite, we might use the `Effect` monad to compose multiple tests in our `main` function.

Run the tests using `spago test`, and you should hopefully see "Tests succeeded." in the last line.

## Creating Executables

We can modify the `main` function in the `src/Main.purs` module to print our result to the console:

    module Main where

    import Prelude

    import Euler (answer)
    import Effect.Console (log)

    main = do
    log ("The answer is " <> show answer)

The `spago run` command can be used to compile and run the `Main` module:

    $ spago run
    [info] Build succeeded.
    The answer is 233168

## Compiling for the Browser

Spago can be used to turn our PureScript code into JavaScript suitable for use in the web browser by using the `spago bundle-app` command:

    $ spago bundle-app
    ...
    Build succeeded.
    Bundle succeeded and output file to index.js

All the code in the `src` directory and any project dependencies have been compiled to JavaScript. The resulting code is bundled as `index.js` and has also had any unused code removed, a process known as dead code elimination. This `index.js` file can now be included in an HTML document.  Try this by creating the following `index.html` file in your `my-project` directory:

    <!DOCTYPE html>
    <html>

    <head>
    <meta charset="UTF-8">
    <title>Euler Exercise</title>
    </head>

    <body>
    <script src="./index.js"></script>
    </body>

    </html>
gss
See this HTML file work as follows:

1. Run `npx parcel index.html --open` in a terminal opened to your `my-project`.
1. In the newly-opened browser, browse `localhost:1234`.  You will probably see a blank window.
1. Open a debugger console.
1. Observe the console contains the Euler exercise result string `The answer is 233168`.

## How the PureScript Compiler Generates Javascript

If you open `index.js`, you should see a few compiled modules which look like this:

    // Generated by purs bundle 0.13.6
    var PS = {};

    // ... (skipped over code)

    (function($PS) {
    "use strict";
    $PS["Euler"] = $PS["Euler"] || {};
    var exports = $PS["Euler"];
    var Data_EuclideanRing = $PS["Data.EuclideanRing"];
    var Data_Foldable = $PS["Data.Foldable"];
    var Data_List = $PS["Data.List"];
    var Data_List_Types = $PS["Data.List.Types"];
    var Data_Semiring = $PS["Data.Semiring"];
    var ns = Data_List.range(0)(999);
    var multiples = Data_List.filter(function (n) {
        return Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(n)(3) === 0 || Data_EuclideanRing.mod(Data_EuclideanRing.euclideanRingInt)(n)(5) === 0;
    })(ns);
    var answer = Data_Foldable.sum(Data_List_Types.foldableList)(Data_Semiring.semiringInt)(multiples);
    exports["answer"] = answer;
    })(PS);

    (function($PS) {
    // Generated by purs version 0.13.6
    "use strict";
    $PS["Main"] = $PS["Main"] || {};
    var exports = $PS["Main"];
    var Data_Show = $PS["Data.Show"];
    var Effect_Console = $PS["Effect.Console"];
    var Euler = $PS["Euler"];
    var main = Effect_Console.log("The answer is " + Data_Show.show(Data_Show.showInt)(Euler.answer));
    exports["main"] = main;
    })(PS);

    PS["Main"].main();

This illustrates a few points about the way the PureScript compiler generates JavaScript code:

- Every module gets turned into an object, created by a wrapper function, which contains the module's exported members.
- PureScript tries to preserve the names of variables wherever possible.
- Function applications in PureScript get turned into function applications in JavaScript.
- The main method is run after all modules have been defined and is generated as a simple method call with no arguments.
- PureScript code does not rely on any runtime libraries. All of the code that is generated by the compiler originated in a PureScript module somewhere which your code depended on.

These points are important since they mean that PureScript generates simple, understandable code. The code generation process, in general, is quite a shallow transformation. It takes relatively little understanding of the language to predict what JavaScript code will be generated for a particular input.

## Compiling CommonJS Modules

Spago can also be used to generate CommonJS modules from PureScript code. This can be useful when using NodeJS, or just when developing a larger project which uses CommonJS modules to break code into smaller components.

To build CommonJS modules, use the `spago build` command:

    $ spago build
    ...
    Build succeeded.

The generated modules will be placed in the `output` directory by default. Each PureScript module will be compiled to its own CommonJS module, in its own subdirectory.

## What Next?

If you're new to typed functional programming, your next stop should be [PureScript by Example](https://book.purescript.org/), which will walk you through learning PureScript by solving practical problems.

If you are already familiar with an ML-family language, like Haskell or Elm, PureScript by Example should still be appropriate as a starting point, but you may alternatively want to start by browsing the [language reference in the documentation repository](https://github.com/purescript/documentation/tree/master/language) instead. The language reference gives a more brief, reference-style description of the language, and is aimed at those who are already somewhat familiar with typed functional programming. There is also a [Differences from Haskell](https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md) page which Haskell programmers will find useful.

New PureScript programmers are also encouraged to spend some time browsing [Pursuit](https://pursuit.purescript.org), which hosts generated API documentation for PureScript libraries. In particular it is worth familiarising yourself with the [core libraries](https://github.com/purescript) (i.e., those which are hosted under the `purescript` organisation on GitHub), and especially the [prelude](https://pursuit.purescript.org/packages/purescript-prelude), as these provide many basic concepts which are frequently useful for writing programs.
