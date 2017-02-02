# `CannotWriteFile` Error

## Example

```text
$ pulp build

Compiling Main
Error found:

  Unable to write file:

    output/Main/index.js

See https://github.com/purescript/purescript/wiki/Error-Code-CannotWriteFile for more information,
or to contribute content related to this error.
```

## Cause

The `CannotWriteFile` error is thrown when `psc`, `psci` or `psc-ide-server` cannot write to the filesystem. `psc` writes to various files, but this usually occurs when trying to write out compiled JavaScript files.

## Fix

- Make sure the target directory (usually `output`) exists and is writable.
- If you are using `psc-ide-server` then try restarting the process before retrying.
- Try deleting the `output` directory and building again. If you are using `psci`, then you might like to try deleting the `.psci_modules` directory.

## Notes
