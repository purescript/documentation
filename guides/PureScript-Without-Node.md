The PureScript community has standardized on the NodeJS toolset to a certain extent: we use tools like Pulp, Browserify and Gulp, which run on NodeJS and are installed with NPM, and we use Bower for our package management. However, we have some users who would rather not, or cannot, run NodeJS on their machines. In this short post, I'll demonstrate how to use the PureScript compiler directly on the command line, and how to automate some processes using standard tooling.

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

import Effect (Effect)
import Effect.Console (log)
import Data.Unit (Unit)

main :: Effect Unit
main = log "Hello, World!"
```

#### Installing Dependencies

Instead of using Bower, we'll need to install dependencies manually. Let's fetch source packages using `git`. For this demo, I'll need `purescript-console`, which has dependencies on `purescript-effect` and `purescript-prelude`:

```text
$ mkdir dependencies/
$ git clone --branch v4.1.0 git@github.com:purescript/purescript-prelude.git dependencies/purescript-prelude/
$ git clone --branch v2.0.0 git@github.com:purescript/purescript-effect.git dependencies/purescript-effect/
$ git clone --branch v4.1.0 git@github.com:purescript/purescript-console.git dependencies/purescript-console/
```

Note that, without Bower as our dependency manager, it's necessary to install all dependencies manually, including transitive dependencies. It's also necessary to manage versions by hand.

#### Building Sources

Now we can build our project. It is necessary to provide all source files explicitly to `purs compile`. The simplest way is to use a set of globs:

```text
$ purs compile src/Main.purs 'dependencies/purescript-*/src/**/*.purs'
```

Note: FFI files are automatically discovered when a PureScript file uses `foreign import` and are not required in the globs.

If everything worked, you should see a collection of lines like this

```text
Compiling Effect
Compiling Effect.Class
Compiling Effect.Console
...
```

`purs` will write a collection of CommonJS modules to the output directory. You can use these CommonJS modules in NodeJS, but it is more likely that you will want to bundle them for use in the browser.

#### Bundling JavaScript for the Browser

To bundle the generated Javascript code into a single file, we will use `spago bundle`.

`spago bundle-app` take the module with the main as input, and generates a single self-contained JavaScript file.

Run `spago bundle-app` with the following arguments:

```bash
$ spago bundle-app --main Main --to index.js
```

The `--main` argument will make the output JavaScript file an executable by adding a line of JavaScript to it which runs the `main` function in the specified module.

If you want to bundle for Node.js, you can use the following command:

```bash
spago bundle-app --main Main --to index.js --platform node
```
You should be able to execute the generated JavaScript using Node.js.

For more information, see the [spago documentation](https://www.github.com/purescript/spago).

#### Conclusion

I've shown how it's possible to get started with PureScript without using the tools from the Node.JS ecosystem. Obviously, for larger projects, it takes some work to manage library dependencies this way, which is why the PureScript community has decided to reuse existing tools. However, if you need to avoid those tools for any reason, it is possible to script some standard tasks without them.
