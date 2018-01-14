# `ImplicitQualifiedImport` Warning

## Example

```purescript
module ShortFailingExample where

import Data.Function as F
import Data.Functor as F
```

## Cause

Multiple modules have been imported implicitly with the same qualifier.

## Fix

Use a distinct qualifier, or supply an explicit import list for all but one of the imports in question.

## Notes

As with unqualified implicit imports, the use of the same qualifier for multiple imports without an explicit import
list, the resolution of an identifier may be ambiguous. Just as with unqualified imports, this means that the addition
of an identifier in a dependency may be a breaking change.

[This error is auto-fixable](../guides/Error-Suggestions.md).
