Add the export to the list of exported functions.

E.g. if getting:

```text
Error in module M:

An export for foo requires the following to also be exported:
    
Bar
```

for

``` haskell
module M (foo) where

import Prelude

foo :: Bar -> Bar
foo x = x
```

The problem is that PureScript requires you to export any types involved in the type signature for `foo`. Similar restrictions apply to things like type classes and type class member exports.

Add `Bar` to the export list: 

``` haskell
module M (foo, Bar) where

import Prelude

foo :: Bar -> Bar
foo x = x
```

See the [language guide page on modules](Language-Guide:-Modules#importing-modules.md).
