# `ScopeConflict` Error

## Example

```purescript
module ScopeConflict where

-- `Prelude` exports a function called `id`
import Prelude

id :: Number
id = 1
```

## Cause

The `ScopeConflict` error occurs when an attempt is made to define a value or other name when the same name is already in scope.

## Fix

- Hide the problematic imports with a `hiding` list:

    ```purescript
    module NoScopeConflict where

    import Prelude hiding (id)

    id :: Number
    id = 1
    ```

- Alternatively, import the conflicting module using the `as` keyword:

    ```purescript
    module NoScopeConflict where

    import Prelude as P

    id :: Number
    id = 1
    ```

## Notes

A common case this arises at the moment is if PureScript 0.11.x libraries are used with the 0.10.x compiler. The resulting error is:

```
Conflicting definitions are in scope for kind Effect from the following modules:

    Control.Monad.Eff
    Prim
```

To fix this either downgrade the libraries, or upgrade the compiler.
