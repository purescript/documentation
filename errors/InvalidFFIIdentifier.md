# `InvalidFFIIdentifier` Error

## Example

```purescript
module ShortFailingExample where

...
```

## Cause

- The exported identifier might not be a valid purescript identifer.
- Suggest other causes.

## Fix

- Purescript identifiers must start with a lowercase character or `_`.

## Notes

- See the lexing rules here: https://github.com/purescript/purescript/blob/fe67ac07e9cac1d361a716740d8c82020b7a1214/lib/purescript-cst/src/Language/PureScript/CST/Lexer.hs#L684

