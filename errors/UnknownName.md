# `UnknownName` Error

## Example

```
> import Prelude
> let bar x = y + 1

Error found:
in module $PSCI
at line 1, column 13 - line 1, column 14

  Unknown value y
```

## Cause

This error if a name is used but that name is not in scope. Names can refer to various things - values, types, operators, type classes, etc.

## Fix

- Verify the spelling of any names used.
- Verify that the name has been imported.
- If you are using qualified imports, make sure the module name is correct.

## Notes
