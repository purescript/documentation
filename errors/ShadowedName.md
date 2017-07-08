# `ShadowedName` Warning

## Example

```
> x = let x = 1 in x

Warning 1 of 1:

  at line 1, column 1 - line 3, column 7

    Name x was shadowed.

  in value declaration x
```

## Cause

This warning occurs when a name is brought into scope, but that name is already defined in the current scope. In the example above, `x` is used to refer to both the inner and outer `let` binding.

PureScript warns in this case because multiple uses of the same name might make it easy to refer to the wrong value.

## Fix

- Consider renaming one or other occurrence of the name.

## Notes
