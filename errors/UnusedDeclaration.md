# `UnusedDeclaration` Warning

## Example

```purescript
module ShortFailingExample ( exportedThing ) where

-- This declaration is exported, so used
exportedThing = 42 :: Int

-- This declaration is neither used in the module, nor exported
problem = "problem" :: String

```

## Cause

This warning identifies a declaration that is not exported, and is not used in the containing module. Such a declaration is "dead code" and can safely be removed.

## Fix

- Remove the declaration if it is unused
- Use it within the module
- Export the declaration if required

## Notes
