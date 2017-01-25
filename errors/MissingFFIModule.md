# `MissingFFIModule` Error

## Cause

This error occurs if a module uses any foreign imports, but a foreign module could not be found.

Usually, this means that you declared a foreign import in the PureScript module but didn't create a corresponding FFI module yet.

## Fix

- Check that the foreign module with the matching name exists. The PureScript compiler finds FFI modules by taking the source file path, and replacing the `.purs` extension with `.js`.

## Notes
