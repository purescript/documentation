# Webpack

> At its core, webpack is a static module bundler for modern JavaScript applications. When webpack processes your application, it recursively builds a dependency graph that includes every module your application needs, then packages all of those modules into one or more bundles.
> - https://webpack.js.org/concepts/

For more information about webpack, read their documentation:

- [webpack concepts](https://webpack.js.org/concepts/)
- [webpack configuration](https://webpack.js.org/configuration/)
- [webpack API](https://webpack.js.org/api/)
- [webpack guides](https://webpack.js.org/guides/)
- [webpack loaders](https://webpack.js.org/loaders/)
- [webpack plugins](https://webpack.js.org/plugins/)

Following is a quick-start guide to using webpack.

## Usage Introduction

Install webpack using their [installation guide](https://webpack.js.org/guides/installation/) or a different method of your choice. You might also need to install a "webpack-cli" package.

Webpack supports configuration by command-line arguments and by configuration file, but because many configuration options can't be defined by command-line arguments, it's recommended to use a configuration file. Create a webpack configuration file by entering the following into a "webpack.config.js" file in the root of your project directory.

``` JavaScript
// webpack.config.js

module.exports = {
  // Enter here. If the name of your PureScript entry module is named "Main",
  //   by default it will be output to the "./output/Main/index.js" file.
  entry: './output/Main/index.js',
  // Output the bundled program to "bundle.js" file in the current directory.
  output: {
    filename: 'bundle.js',
    path: __dirname
  }
};
```

Then execute webpack.

``` sh
# If you installed from NPM
$ ./node_modules/.bin/webpack
```

Webpack should output something like the following. You can see it produced one asset, called bundle.js, which it called "Chunk number 0", and you can see each module which was included in that bundle.

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