# `DeprecatedFFIPrime` Warning

## Example

```javascript
exports["example'"] = 0;
```

```purescript
module ShortFailingExample where

foreign import example' :: Int
```

## Cause

An identifier imported from a foreign module contains a prime (`'`) character.

## Fix

Remove the prime from the identifier:

```diff
-exports["example'"] = 0;
+exports.example = 0;
```

```diff
module ShortFailingExample where

-foreign import example' :: Int
+foreign import example :: Int
```
