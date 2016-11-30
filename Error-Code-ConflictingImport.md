This error can occur when you attempt to define a value with the same name as one which has already been imported.

For example, `Prelude` exports a function called `id`. If I attempt to define my own `id` as well, like this:

```purescript
module ConflictingImport where

id :: Number
id = 1
```

Then this will fail with:

```
>> Error in module ConflictingImport:
>> Declaration id conflicts with import ConflictingImport
```

One potential solution is to hide the problematic imports with a `hiding` list:

```purescript
module NoConflictingImport where

import Prelude hiding (id)

id :: Number
id = 1
```

Alternatively, we can import one of the two modules which define `id` as "qualified":

```purescript
module NoConflictingImport where

import qualified Prelude as P

id :: Number
id = 1
```
