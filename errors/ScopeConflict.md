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
