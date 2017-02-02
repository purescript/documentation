# `WildcardInferredType` Warning

## Example

```purescript
test :: _
test = [1, 2, 3]
```

## Cause

This warning is shown when you use a _type wildcard_ in your code. In the example above, the type of `test` is given as a wildcard. The warning will show the inferred type of the wildcard as `Array Int`.

This is an encouragement to document your code by using complete type signatures, but it will not cause any errors.

## Fix

- To remove the warning, replace the type wildcard with the suggested type. Some editor plugins provide the ability to do the replacement automatically.

## Notes
