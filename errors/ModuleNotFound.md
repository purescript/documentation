# `ModuleNotFound` Error

## Example

```purescript
module Example where

-- Notice the typo here: the module we are after is called Control.Monad.Eff.Console.
import Contorl.Monad.Eff.Console (log)

main = log "Hello, world!"
```

## Cause

You might see this error because you have attempted to import a module which the compiler was unable to find, or because you mistyped the name of a module.

## Fix

Check that:

- you have spelled the name of the module correctly,
- if the module comes from a library, that you have installed that library in your project,
- you have supplied the filename of the module to `purs build` on the command line (note that build tools, such as `webpack`, `parcel` or `spago`, should generally take care of this for you; if in doubt, check their documentation).

If you know which module you are looking for but are unsure which library it comes from, you can search for it on <https://pursuit.purescript.org>.
