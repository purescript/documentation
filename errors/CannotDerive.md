# `CannotDerive` Error

## Example

```purescript
data Bool = True | False

derive instance showBool :: Show Bool
```

## Cause

This error shows up when you're attempting to derive an instance for which compiler
support does not exist.

## Fix

- You will need to write an instance yourself.
- If you want to derive e.g. `Show` you can use [Generics](https://github.com/purescript/documentation/blob/master/guides/Generic.md#show-eq-ord) like this:
  - install `purescript-generics-rep`
  - ```purescript
       derive instance genericBool :: Generic Bool _
       
       instance showBool :: Show Bool where show = genericShow
     ```
