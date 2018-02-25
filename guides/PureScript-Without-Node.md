
The PureScript community has standardized on tools like Pulp, Bower, Browserify, Webpack, and RollupJS, all of which use the NodeJS runtime and use NPM as their supported distribution channel. There are users who cannot, or prefer not to, use NodeJS-based tools. PureScript can be used without NodeJS-based tools.

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

We can use any dependency management strategy that doesn't rely on a NodeJS-based tool. Try using one of these techniques:

- [Using Git for PureScript Dependency Management page](guides/Dependency-Management/Git.md)
- [Using PSC-Package for PureScript Dependency Management page](guides/Dependency-Management/PSC-Package.md)


```text
$ mkdir dependencies/
$ git clone --branch v0.1.0 git@github.com:purescript/purescript-prelude.git dependencies/purescript-prelude/
$ git clone --branch v0.1.0 git@github.com:purescript/purescript-eff.git dependencies/purescript-eff/
$ git clone --branch v0.1.0 git@github.com:purescript/purescript-console.git dependencies/purescript-console/
$ git clone --branch v3.3.0 git@github.com:purescript/purescript-invariant.git ps-deps/purescript-invariant/
```

Note that, without Bower as our dependency manager, it's necessary to install all dependencies manually, including transitive dependencies. It's also necessary to manage versions by hand.

#### Building Sources

It is necessary to provide all source files explicitly to the PureScript compiler. The simplest way is to use a set of file path globs.

```text
$ purs compile 'src/**/*.purs' 'ps-deps/purescript-*/src/**/*.purs'
```

By default, `purs compile` will write a collection of CommonJS-formatted JavaScript modules to the `output` directory.

If you want to execute this resulting JavaScript in a web browser, you'll need to perform some post-compilation steps, such as bundling. See the [Bundling for a Web Browser guide](guides/Bundling-for-Web-Browser.md) to learn how to do this.
