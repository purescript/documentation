# `DeprecatedFFICommonJSModule` Error

## Example

```javascript
const mymodule = require('some-module')

exports.myvar = mymodule.myvar
...
```

## Cause

This error occurs when CommonJS (ES5) is used in FFI instead of ES modules (ES6). Support for CommonJS support has been removed in v0.15.0 and all FFI need to be written as ES modules instead.

## Fix

- Change `require` to `import`: 

    ```javascript
    const mymodule = require('some-module')
    ```
  to
    ```javascript
    import mymodule from 'some-module'
    ```
- Change `exports.name = value;` to `export const name = value;`

## Notes

Various tools exist to migrate ES5 code to ES6 code automatically. 

One tool we recommend is [lebab](https://github.com/lebab/lebab).
You can do just that transformation with one of the following commands:
```bash
# replace all *.js files in the src directory by rewriting them from
# commonjs modules to es modules
lebab --replace src --transform commonjs

# you can also provide glob patterns, if you would like
lebab --replace 'src/js/**/*.jsx' --transform commonjs
```

The CommonJS → ES modules transform is considered unsafe, because there are some edge cases the tool is unable to handle. These are issues worth checking for in your updated code:
https://github.com/lebab/lebab#unsafe-transforms
