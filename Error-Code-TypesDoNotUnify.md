```
Error checking that type
  Control.Monad.Eff.Eff (assert :: Test.Assert.ASSERT | _0) Prelude.Unit
subsumes type
  Control.Monad.Eff.Eff (a :: Test.Assert.ASSERT | e0) Prelude.Unit
Error at /.../Main.purs line 7, column 3 - line 7, column 10:
  Cannot unify type
    (assert :: Test.Assert.ASSERT | _0)
  with type
    (a :: Test.Assert.ASSERT | e0)
```

Even though the type of the effect (`Test.Assert.ASSERT`) is correct, the name does not match. The effect name *must* match. So to fix this error, change `a` to `assert`. 

*When using Halogen*

If you are seeing this error in relation to the definition of a component try checking the type signatures  of the component and child component's slot addresses, query types and state types against the examples in the purescript halogen repository.

*When using Halogen without components* (this is a question)

The following code produces this error:

```
module Broken1 where

import Control.Monad.Aff.Class (MonadAff)
import Control.Monad.Aff (Aff())
import DOM.HTML.Types (HTMLElement())
import Halogen (Component(), HalogenEffects(), Natural(), runUI, component)
import Prelude

import qualified Halogen.HTML.Indexed as H

data State = State
data Query a = Query a

ui :: forall eff g. (MonadAff (HalogenEffects eff) g) => Component State Query g
ui = component (\_ -> H.div_ []) (\(Query next) -> pure next)

main' :: forall eff a. (HTMLElement -> Aff (HalogenEffects eff) a) -> Aff (HalogenEffects eff) Unit
main' addToDOM = do
    { node: node, driver: driver } <- runUI ui State

    let driver' :: Natural Query (Aff (HalogenEffects eff))
        driver' = driver

    return unit
```

If I omit the type signature for `driver'`, things are fine.  If I ask `psc` for a signature, I will get roughly the same one (except for type synonym replacement and anonymous variable renamings), and if I cut&paste that into the code, I get the same error again.

Any help appreciated!  Hope it's ok to abuse these pages for support requests.  (-: