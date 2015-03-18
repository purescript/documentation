Installation
---------------------

Compiler binaries are available on [the download page](http://www.purescript.org/download/), and through several Mac and Linux package managers, including Homebrew, Arch, and Nix.

### Compiling from source

If you have Haskell Plaform installed, then you can install the latest released version from Hackage:
```sh
cabal update
cabal install purescript
```
If you would like to build the latest version of the code, clone this repository and build:
```sh
git clone git@github.com:purescript/purescript.git
cd purescript
cabal configure --enable-tests
cabal build
cabal test
cabal install
```
Alternatively, consider installing PureScript in a Cabal sandbox using ``cabal sandbox init``.

Creating a new PureScript project
---------------------------------
### Pulp
The simplest way to start writing PureScript is with [pulp](https://github.com/bodil/pulp), a build tool for PureScript, available through Node.js (install with `npm install -g pulp`). It will initialize, install dependencies for, build and optimize, and even create documentation for a project.
```sh
pulp init
pulp dep install purescript-tuples
pulp build -O --to foo.js
pulp docs > Foo.md
```

However, `pulp`'s duties stop there; loading your code into some HTML is up to you! Consider a shim file, like:
```html
<!DOCTYPE HTML>
<html><head><meta charset="UTF-8"/><title>Foo</title>
</head><body><script async src="foo.js"></script></body></html>
```

### Task runners
Complex projects with non-PureScript tasks to run (e.g. compiling Stylus/SASS/LESS to CSS, compiling Jade/CoffeeCup/Blaze to HTML) usually rely on a *task runner*.

#### Grunt
You can create a new PureScript project using ``grunt-init`` and [this PureScript project template](https://github.com/purescript-contrib/grunt-init-purescript). This will give you a project that uses [Grunt](http://gruntjs.com) as its build tool and [Bower](http://bower.io) for dependency management. See the [README](https://github.com/purescript-contrib/grunt-init-purescript) for step-by-step instructions.

#### Other alternatives
If you don't want to use Grunt, you might consider [gulp-purescript](https://github.com/purescript-contrib/gulp-purescript) or [generator-purescript](https://github.com/joneshf/generator-purescript) for Yeoman.

Compiler usage
--------------

The `psc` executable takes a list of PureScript source files as arguments and by default writes out its errors or output to the console.

The following options are supported:

Option                 | Description
-----------------------|----
--stdin                | Read input from standard input instead of from files.
--output               | Write the generated Javascript to the specified file.
--externs              | Write a list of foreign imports declarations to the specified file in addition to generating Javascript output.
--no-tco               | Turn off tail-call elimination.
--no-prelude           | Do not include the Prelude in the generated Javascript.
--no-magic-do          | Turn off optimizations which inline calls to ``>>=`` for the ``Eff`` monad.
--no-opts              | Disable all optimizations.
--main                 | Generate a call to ``main`` in the specified module after all other generated Javascript. Defaults to ``Main`` if the option is used but no value is provided.
--module               | If specified, any code which is not referenced transitively from this module will be removed. This argument can be used multiple times.
--codegen              | A list of modules for which Javascript and externs should be generated. This argument can be used multiple times.
--browser-namespace    | Specify the namespace that PureScript modules will be exported to when running in the browser.
--verbose-errors       | Generate verbose error messages.

psc-make
--------

The ``psc-make`` executable makes CommonJS modules and supports incremental compilation. Unlike ``psc``, it does not do dead code elimination.

The following command line options are supported:

Option                 | Description
-----------------------|----
--output=VAL           | The output directory. Default: ``output``.
--verbose-errors       | Display verbose error messages.
--help=FMT             | Show this help in format FMT (``pager``, ``plain``, or ``groff``). Default: ``pager``.
--no-magic-do          | Disable the optimization that overloads the ``do`` keyword to generate efficient code specifically for the ``Eff`` monad.
--no-opts              | Skip the optimization phase.
--no-prelude           | Omit the Prelude.
--no-tco               | Disable tail call optimizations
