# `TransitiveExportError` Error

## Example

```purescript
module Example (foo) where

data Bar

foo :: Bar -> Bar
foo x = x
```

## Cause

This error occurs when one name is exported, but exporting that name requires some other name(s) to also be exported.

The example above generates the following error:

```text
Error in module M:

An export for foo requires the following to also be exported:

Bar
```

PureScript requires any types appearing inside a type declaration to also be exported. Similar restrictions apply to things like type classes and type class member exports.

## Fix

- Follow the instructions in the error, and add the required names to the list of exports:

    ```purescript
    module Example (foo, Bar) where

    data Bar

    foo :: Bar -> Bar
    foo x = x
    ```

## Notes

See the [language guide page on modules](../language/Modules.md#module-exports).
