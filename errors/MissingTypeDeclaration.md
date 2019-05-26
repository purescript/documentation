# `MissingTypeDeclaration` Warning

## Example

```purescript
module Main where
import Effect.Console (log)

main = log "No type annotation has been provided."
```

## Cause

This warning is issued when a top-level declaration does not have a type annotation. The vast majority of types in PureScript code are inferrable, so it's relatively rare that the absence of a type annotation has much of an effect on compilation, but it is considered good practice to add annotations to top-level declarations anyway (as a form of documentation).

## Fix

Add a type annotation:

```purescript
module Main where
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = log "A type annotation has been provided!"
```

## Notes

Many of [the PureScript text editor plugins](/ecosystem/Editor-and-tool-support.md) can save you the effort of writing type annotations by filling in the types for you.

Declarations which are not at the top level, such as those contained within `let` or `where` clauses, are not considered as important to annotate with types, and so the compiler does not issue warnings when these declarations are not annotated.  The following example compiles without warnings, even though `message` has not been given a type annotation:

```purescript
module Main where
import Effect.Console (log)

main :: Effect Unit
main =
  let
    message = "Hello, world!"
  in
    log message
```
