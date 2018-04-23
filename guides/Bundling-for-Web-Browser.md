# Bundling for a Web Browser

A language's libraries are often maintained as modules separate from any specific application to enable them to be easily re-used across applications. Any JavaScript app which uses shared libraries will be loading specific modules and specific functions in those modules. In JavaScript, each file is treated as a separate module.

In a web browser, loading a dependency is a network request. Remembering that dependencies have dependencies, you can look at any simple JavaScript application and see that it consists of dozens or hundreds of modules. When you run such a JavaScript application in a browser, then, you'll see dozens or hundreds of network requests as it fetches its dependencies.

A bundler will reduce an application's dependency graph by moving a module's dependencies and transitive dependencies into the module itself. This means a web browser does not need to load a JavaScript app's dependencies. If an application wants to make its bundle smaller, some bundlers can split the app into chunks and lazy-load them when they are needed.

## Module Formats

Bundlers can export the bundle in one of several JavaScript module formats. It's also important to know that some bundlers only support importing an application using a specific module format, though you can apply a plugin to add support for other module formats.

Currently, the PureScript compiler outputs CommonJS-formatted JavaScript modules. Other JavaScript module formats include AMD, UMD, ES6, IIFE, and SystemJS. Following is a quick overview of the various module formats.

The CommonJS format offers synchronous module loading and is the de facto module format of modules used by the NodeJS ecosystem.

The AMD format was invented to support loading modules in a web browser asynchronously across the network.=

The UMD format unifies the CommonJS and AMD formats to enable modules in its format to be loaded as a CommonJS module or as an AMD module, at the cost of a slightly more incomprehensible module format.

The IIFE format isn't a complete module system, as it doesn't define a way of loading dependencies, but it is still useful as a format to use for a top-level module, the one first loaded and executed by a web browser, as it uses a JavaScript closure to ensure only specifically exported values are exported and arranges them onto a single object, rather than independently defining each export on the global scope.

The ES6 format is the new JavaScript standard module format, but it is not currently not supported by most web browsers. You can use this format during development but will need to transpile to a different format when deploying to production.

SystemJS is a module format which closely follows the ECMAScript specifications, but because dynamic loading isn't included in the ES6 specification yet SystemJS offers some guidance as a reference implementation, and can perhaps be called a polyfill, until it's added to the ECMAScript specification and all major browsers implement it.


## Bundling

To run a JavaScript app which uses CommonJS-formatted modules in a web browser requires compiling it to a module format supported by a web browser. In simple browser apps, the resulting output will likely be just a single, very big JavaScript file with modules removed or wrapped in a similar implementation of CommonJS modules.

The programs which perform this compilation process are generally called "bundlers". The first program which did this was [Browserify](http://browserify.org/) but many alternative bundlers have been created since then. Currently the most popular and well-supported bundler is [Webpack](https://webpack.js.org/), but other fine options include [RollupJS](https://rollupjs.org/) and [ParcelJS](https://parceljs.org/). Generally, a bundler works by specifying an entrypoint JavaScript file, a desired output module format, and other functions to perform while the bundler traverses and bundles modules, called plugins.

See the documentation provided by those tools for full information of how to use them. As an introduction, we'll see a basic example of these tools.

Note that these tools will simply bundle a JavaScript file's dependencies into that file. A JavaScript file produced by a PureScript module will only define functions, not execute them. If you pass such a file into a bundler and simply load it into a browser, you might be disappointed to see that your project's `main` function isn't executed. To execute it in the browser requires a JavaScript file which imports the `main` function and executes it. The make your project executable, follow the bundler examples below while replacing `output/Main/index.js` with a JavaScript which executes your project. Following is an example JavaScript file which does this, here called `main-runner.js`.

``` JavaScript
// main-runner.js

// Using ES6 modules, if your bundler expects ES6 modules.
import { main } from "./output/Main/index.js"
main();

// Or using CommonJS module, if your bundler expects CommonJS modules.
// var Main = require("./output/Main/index.js");
// Main.main();
```

### Tools

- [webpack quick-start](guides/Bundling/Webpack.md)
- [Rollup quick-start](guides/Bundling/Rollup.md)


## Note on ES6 modules

ECMAScript 6, the latest version of the JavaScript language, adds a module system which is similar to CommonJS but is slightly restricted to enable it to be statically analyzed. Static analysis enables an ES6 JavaScript module's dependencies to be asynchronously loaded prior to running the module, which enables the ES6 module to use its dependencies in a synchronous manner. PureScript currently doesn't output ES6-formatted modules, partly because not all browsers completely support ES6 and partly because its FFI parser doesn't support parsing ES6. ES6 modules are enticing because they remove the need to perform a bundling step.

A relevant topic here is loading a dynamically-chosen dependency, a commonly-used feature of the CommonJS module system. This facility is not included in the ES6 specification, but work is underway to add it to a future version of the specification. The suggested syntax for this is `import('./dist/some-module.js').then(({ default: main }) => main());`, compared to CommonJS's `const { default: main } = require('./dist/some-module.js'); main();`. In ES6, the asynchronous nature of this operation surfaces, as we see the operation returns a Promise containing the requested functions from the module's exports.