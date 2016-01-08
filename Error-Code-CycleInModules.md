This is a problem that occurs when you have two modules that depend on each other.

For example, with these two modules:

```purescript
module Foo where
import Bar
...
```

```purescript
module Bar where
import Foo
...
```

The Purescript compiler will spit out:
```
Error found:
  There is a cycle in module dependencies in these modules:

    Foo
    Bar

See https://github.com/purescript/purescript/wiki/Error-Code-CycleInModules for more information,
or to contribute content related to this error.
```