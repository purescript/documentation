# Using Git for PureScript Dependency Management

It's possible to use Git as a dependency management tool. You'll need to go find the PureScript library repos yourself, find the release tag you want, and clone them into a directory in your PureScript project. Note that you'll need to also manually install the dependencies of your desired direct dependencies, which are called transitive dependencies. Then, pass the paths to these Purescript libraries to the compiler when building your project.

For example:

``` sh
# In a PureScript project's directory
$ mkdir ps-deps/
$ git clone --branch v3.1.1 git@github.com:purescript/purescript-prelude.git ps-deps/purescript-prelude/
$ git clone --branch v3.2.0 git@github.com:purescript/purescript-eff.git ps-deps/purescript-eff/
$ git clone --branch v3.3.0 git@github.com:purescript/purescript-monoid.git ps-deps/purescript-monoid/
$ git clone --branch v3.3.1 git@github.com:purescript/purescript-control.git ps-deps/purescript-control/
$ git clone --branch v3.0.0 git@github.com:purescript/purescript-invariant.git ps-deps/purescript-invariant/
$ git clone --branch v2.0.0 git@github.com:purescript/purescript-newtype.git ps-deps/purescript-newtype/
$ git clone --branch v3.0.0 git@github.com:purescript/purescript-console.git ps-deps/purescript-console/
# Include them when building
$ purs compile 'src/**/*.purs' 'ps-deps/purescript-*/src/**/*.purs'
```

A PureScript module's FFI files are automatically discovered by the compiler, and therefore are not required as input to `purs compile`.

If everything worked, you should see a series of lines indicating compilation status like:

``` sh
Compiling Control.Monad.Eff
Compiling Control.Monad.Eff.Class
Compiling Control.Monad.Eff.Console
...
```

