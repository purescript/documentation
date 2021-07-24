# `OrphanTypeDeclaration` Error

## Example

```purescript
module ShortFailingExample where

addTwo :: Number -> Number -> Number
addToo a b = a + b
...
```

## Cause

Purescript requires that the type declaration for a function or value (i.e. the line with the `::` in it)
be followed immediately by the definition of that function or value (i.e. the line with the `=` in it). The
compiler has found a declaration for which a definition did not follow.

This is often caused by typographical issues.  Check to see that the declaration and the
definition are spelled the same. PureScript is a whitespace sensitive language and the presence of an
extra space can make the definition look like a part of the declaration.

## Fix

- Ensure that the next (non-comment) line is the definition for the listed declaration.
- Check that the spelling of the two matches.
- Check for extra spaces.  Both declaration and definition should start with the same indentation.
