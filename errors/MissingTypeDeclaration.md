# `MissingTypeDeclaration` Error

## Example

```purescript
module Main where
import Prelude
import Effect.Console (log)

main = log "No type annotation has been provided."
```

## Cause

Explain why a user might see this error.

## Fix

- Declare the type
```purescript
module Main where
import Prelude
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = log "The type is now declared!"
```

## Notes

- MissingTypeDeclaration is not an error, it is a warning.
- Type annotations are necessary in cases where you're using type classes in an ambiguous way.
