# Bundling for a Web Browser

Currently, the PureScript compiler outputs CommonJS-formatted JavaScript modules, which is just one of several JavaScript module formats. Other JavaScript module formats include AMD, UMD, ES6, IIFE, SystemJS, and Global. The CommonJS module format was invented by the early NodeJS app ecosystem, as it has a different set of limitations than the web browser app ecosystem. The primary difference that relates to JavaScript module formats is that of loading dependent modules. NodeJS is a customized JavaScript runtime, and one of the features they added to the JavaScript language is support for synchronous module loading. NodeJS can do this while a web browser can not because NodeJS loads dependent modules using simple filesystem access, a relatively low performance cost, whereas a web browser needs to send an HTTP request to the website's server to request subsequent modules, a quite large performance cost.

## Bundling

To run a JavaScript app which uses CommonJS-formatted modules in a web browser, then, requires compiling it to a module format supported by a web browser. In simple browser apps, the resulting output will likely be just a single, very big JavaScript file with modules removed or wrapped in a similar implementation of CommonJS modules.

The programs which perform this compilation process are generally called "bundlers". The first program which did this was [Browserify](http://browserify.org/). Currently the most popular and well-supported bundler is [Webpack](https://webpack.js.org/), but other fine options include [RollupJS](https://rollupjs.org/) and [ParcelJS](https://parceljs.org/). Generally, a bundler works by specifying an entrypoint JavaScript file, a desired output module format, and other functions to perform while bundling, called plugins.

See the documentation provided by those tools for full information of how to use them. As an introduction, we'll see a basic example of these tools.

### Webpack

Install Webpack using their [installation guide](https://webpack.js.org/guides/installation/) or a different method of your choice. You might also need to install a "webpack-cli" package.

Create a webpack configuration file by entering the following configuration in a "webpack.config.js" file in the root of your project directory.

``` JavaScript
module.exports = {
  // Enter here. If the name of your PureScript entry module is named "Main",
  //   by default it will be output to the "./output/Main/index.js" file.
  entry: './output/Main/index.js',
  // Output the bundled program to "bundle.js" file in the current directory.
  output: {
    path: __dirname,
    pathinfo: true,
    filename: 'bundle.js'
  }
};
```

Then execute Webpack.

``` sh
# If you installed from NPM
$ ./node_modules/.bin/webpack
# OR if you installed a different way, execute it your way.
$ PATH=~/path/to/webpack-dir webpack
```

Webpack should output something like the following lines. You can see it produced one asset, called bundle.js, which it called "Chunk number 0", and you can see each module which was included in that bundle.

``` sh
Hash: 8891a2a784e39f88c7ed
Version: webpack 4.0.0
Time: 1690ms
Built at: 2/24/2018 9:12:40 PM
    Asset      Size  Chunks             Chunk Names
bundle.js  24.6 KiB       0  [emitted]  main
Entrypoint main = bundle.js
   [2] ./output/Data.Ring/index.js 1.27 KiB {0} [built]
   [4] ./output/Data.Function/index.js 803 bytes {0} [built]
   [6] ./output/Data.Eq/index.js 1.59 KiB {0} [built]
   [7] ./output/Data.Show/index.js 964 bytes {0} [built]
   [8] ./output/Control.Apply/index.js 2.89 KiB {0} [built]
   [9] ./output/Data.Semigroup/index.js 1.07 KiB {0} [built]
  [10] ./output/Data.EuclideanRing/index.js 3.25 KiB {0} [built]
  [11] ./output/Data.Void/index.js 559 bytes {0} [built]
  [12] ./output/Control.Category/index.js 483 bytes {0} [built]
  [13] ./output/Control.Applicative/index.js 1.88 KiB {0} [built]
  [14] ./output/Data.Ord/index.js 7.89 KiB {0} [built]
  [20] ./output/Prelude/index.js 1.36 KiB {0} [built]
  [24] ./output/Control.Monad.Eff/index.js 1.7 KiB {0} [built]
  [26] ./output/Control.Monad.Eff.Console/index.js 971 bytes {0} [built]
  [47] ./output/Main/index.js 313 bytes {0} [built]
    + 33 hidden modules
```


### RollupJS

!!! To do

## Note on ES6 modules

ECMAScript 6, the latest version of the JavaScript language, enables an ES6 JavaScript module to define its dependencies in a synchronous manner while keeping its ability to be executed in an ES6-compliant web browser. PureScript currently doesn't output ES6-formatted modules because not all browsers completely support ES6 and PureScript wants to output JavaScript which is compatible with all JavaScript runtimes. ES6 modules are enticing because they remove the need to perform a bundling step.
