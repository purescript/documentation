# `CycleInModules` Error

## Example

```purescript
module Foo where

import Bar
```

```purescript
module Bar where

import Foo
```

## Cause

This problem occurs when two modules depend on each other.

For example, given the `Foo` and `Bar` modules above, the compiler will fail with:

```
Error found:
  There is a cycle in module dependencies in these modules:

    Foo
    Bar

See https://github.com/purescript/purescript/wiki/Error-Code-CycleInModules for more information,
or to contribute content related to this error.
```

## Fix

- Break modules into smaller parts, in order to break the cycle of dependencies.
- Alternatively, it might be possible to combine the two modules into a single module.

## Notes
