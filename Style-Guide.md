This page contains guidelines for writing PureScript libraries. You might consider this a set of best practices, or if you would like to contribute to the core libraries, a set of guidelines for getting your pull requests accepted.

### Native Bindings

Purescript is a new language, and so the need to write new libraries is common for the success of your project. When preparing to write a new library, please consider that PureScript needs shared barebones bindings to native apis, as well as higher level libraries that embellish and provide new functionality.

```
Does my library require FFI code into native apis?
  Yes -> Is there already a barebones FFI library in PureScript?
    Yes -> Pull it in as a dependency.
    No  -> Get that barebones bindings library started! Then pull it in as a dependency.
  No -> Awesome, proceed in peace.
```

If you have already written a library that has FFI code into a native api, please consider splitting it into two libraries, (a barebones binding, and your extra functionality). 

### Sharing Your Library

When your library is ready to go. Please share the package on [**Bower**](http://bower.io/search/?q=purescript) prefixing the name of your library with `purescript-` so other PureScripters can find your work. 

Some things to note:
  - Bower works with git tags, but does not require any present tags to publish. Please run `bower version 0.0.0` and push tags for your initial release (this prevents interim work not intended for publication from leaking on 0.0.0 libs).
  - Considering that you may need to be editing your library live as a part of the development of your main project, check out `bower link` (learn all about it [here](https://oncletom.io/2013/live-development-bower-component/)). This will enable you to keep the repos in sync as you work, and facilitate publishing when ready.

### Node Compatibility

Node.js and the Browser have differences. Its desirable for any libraries up on Bower (even though Bower is usually considered Browser-centric) to be compatible with both Node.js and the Browser. If your code is necessarily Node.js or Browser specific, please note this in your `README.md`.

### Documentation Generation

Please use documentation generation and post it prominently in your repository, preferably as the README.md. This can be accomplished with the `psc-docs` command, or with [Gulp-PureScript](https://github.com/purescript-contrib/gulp-purescript#purescriptpscdocsoptions) or [Grunt-PureScript](https://github.com/purescript-contrib/grunt-purescript#the-pscdocs-task). 

### Tests

Tests are strongly preferred, particularly if you are using FFI for bindings in some way. Checkout to following packages to assist your testing efforts:

purescript-test-unit [Test.Unit](https://github.com/bodil/purescript-test-unit)
purescript-featurespec [Test.FeatureSpec](https://github.com/joneshf/purescript-featurespec)
purescript-quickcheck [Test.QuickCheck](https://github.com/purescript/purescript-quickcheck)
purescript-strongcheck [Test.StrongCheck](https://github.com/purescript-contrib/purescript-strongcheck)

If your goal is to use standard js libs for testing and so be able to plug into existing tools like [Karma](http://karma-runner.github.io/0.12/index.html). Consider these as well:

purescript-mocha [Test.Mocha](https://github.com/CapillarySoftware/purescript-mocha)
purescript-chai [Test.Chai](https://github.com/CapillarySoftware/purescript-chai)

### Travis

Even without tests, using Travis to demonstrate that your library type checks and compiles, is of value. To get your project building on Travis can be as simple as adding the following `.travis.yml` file to your project:

```
language : haskell
ghc : 7.8.3
install :
  - 'cabal install purescript'
  - 'bower install'
script : 
  - 'psc-make'
```

Don't forget to display your Travis "Build Passing" badge on your `README.md`, so others can download your package with an inflated sense of confidence. 

### Namespacing

TBD