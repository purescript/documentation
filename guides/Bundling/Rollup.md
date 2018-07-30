# Rollup

> Rollup is a module bundler for JavaScript which compiles small pieces of code into something larger and more complex, such as a library or application. It uses the new standardized format for code modules included in the ES6 revision of JavaScript, instead of previous idiosyncratic solutions such as CommonJS and AMD. ES6 modules let you freely and seamlessly combine the most useful individual functions from your favorite libraries. This will eventually be possible natively, but Rollup lets you do it today.
> - https://rollupjs.org/guide/en

For more information about Rollup, read their documentation:

- [Rollup documentation](https://rollupjs.org/guide/en#introduction)

## Usage Introduction

Install RollupJS using their [quick start guide](https://rollupjs.org/guide/en#quick-start) or a different method of your choice.

Rollup supports configuration by command-line arguments and by configuration file, but because some configuration options can't be defined by command-line arguments, such as plugins, it's recommended to use a configuration file. Create a RollupJS configuration file by entering the following into a "rollup.config.js" file in the root of your project directory.

``` JavaScript
// rollup.config.js

// RollupJS supports only ES6 modules.
// PureScript outputs CommonJS modules and refers to other PS modules using Node-style
//   module refereneces, so we need to add support for this to Rollup using two plugins.
import commonjs from 'rollup-plugin-commonjs';
import nodeResolve from 'rollup-plugin-node-resolve';

export default {
  input: 'output/Main/index.js',
  output: {
    file: 'myapp-bundle.js',
    // If you want to make an in-browser executable, you'll need an input like "main-runner.js" and use an output format of "iife".
    format: 'cjs',
    exports: 'named'
  },
  plugins: [
    nodeResolve(),
    commonjs()
  ]
};
```

Then execute RollupJS.

``` sh
# If you installed from NPM
$ ./node_modules/.bin/rollup -c
```

RollupJS should output something like the following. You can see it bundled "output/Main/index.js" and its dependencies into the "myapp-bundle.js" file.

``` sh
$ ./node_modules/.bin/rollup -c

output/Main/index.js → myapp-bundle.js...
created myapp-bundle.js in 874ms
```