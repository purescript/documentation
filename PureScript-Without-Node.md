The PureScript community has standardized on the NodeJS toolset to a certain extent: we use tools like Pulp, Browserify and Grunt, which run on NodeJS and are installed with NPM, and we use Bower for our package management. However, we have some users who would rather not, or cannot, run NodeJS on their machines. In this short post, I'll demonstrate how to use the PureScript directly on the command line, and how to automate some processes using standard tooling.

#### Creating a Project

Create a directory for your project, with `src` and `test` directories for source files and test files:

```text
$ mkdir purescript-without-node
$ cd purescript-without-node
$ mkdir src/ test/
```

Create a file `src/Main.purs` as follows:

```purescript
module Main where

import Control.Monad.Eff.Console (log)

main = log "Hello, World!"
```

#### Installing Dependencies

Instead of using Bower, we'll need to install dependencies manually. Let's fetch source packages using `git`. For this demo, I'll need `purescript-console`, which has dependencies on `purescript-eff` and `purescript-prelude`:

```text
$ mkdir dependencies/
$ git clone --branch v0.1.0 git@github.com:purescript/purescript-prelude.git dependencies/purescript-prelude/
$ git clone --branch v0.1.0 git@github.com:purescript/purescript-eff.git dependencies/purescript-eff/
$ git clone --branch v0.1.0 git@github.com:purescript/purescript-console.git dependencies/purescript-console/
```

Note that, without Bower as our dependency manager, it's necessary to install all dependencies manually, including transitive dependencies. It's also necessary to manage versions by hand.

#### Building Sources

Now we can build our project. It is necessary to provide all source files, including dependencies and foreign modules (via `--ffi`), explicitly to `psc`. The simplest way is to use a set of globs:

```text
$ psc src/Main.purs \
        'dependencies/purescript-*/src/**/*.purs' \
  --ffi 'dependencies/purescript-*/src/**/*.js'
```

If everything worked, you should see a collection of lines like this

```text
Compiling Control.Monad.Eff
Compiling Control.Monad.Eff.Class
Compiling Control.Monad.Eff.Console
...
```

`psc` will write a collection of CommonJS modules to the output directory. You can use these CommonJS modules in NodeJS, but it is more likely that you will want to bundle them for use in the browser.

#### Bundling JavaScript for the Browser

To bundle the generated Javascript code, we will use the `psc-bundle` tool, which ships with the PureScript compiler binary distribution.

`psc-bundle` takes a collection of CommonJS module files as input, and generates a single JavaScript file.

Run `psc-bundle` with the following arguments:

```text
$ psc-bundle output/*/{index,foreign}.js --module Main --main Main
```

The `--module` argument specifies an _entry-point_ module, which will be used to determine dead code which can be removed. For our purposes, we only care about bundling Javascript code which is a transitive dependency of the `Main` module.

The `--main` argument will generate a piece of Javascript to run our `main` action in the `Main` module, after all other generated code.

If everything worked, you should see about 100 lines of Javascript printed out. This can optionally be redirected to a file with the `-o` argument.

You should be able to execute the generated Javascript in the browser, or using NodeJS.

#### Conclusion

I've shown how it's possible to get started with PureScript without using the tools from the NodeJS ecosystem. Obviously, for larger projects, it takes some work to manage library dependencies this way, which is why the PureScript community has decided to reuse existing tools. However, if you need to avoid those tools for any reason, it is possible to script some standard tasks without them.