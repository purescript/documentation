# `ErrorParsingFFIModule` Error

## Cause

This error occurs when the compiler cannot verify foreign declarations in JavaScript code,
because it was unable to parse a JavaScript file.

## Fix

- Verify the JavaScript code in your foreign modules.
- Foreign modules should conform to the ES5 specification. Values in foreign modules should be provided in the form `exports.name = value;`.

## Notes

- This [article](../guides/FFI.md) explains how to create foreign modules.
- An example can be found [here](https://github.com/purescript/purescript-eff/blob/v1.0.0/src/Control/Monad/Eff.js).
