# `InfiniteType` Error

## Example

```purescript
module InfiniteTypeErrorExample where

import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE, log, logShow)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, runFn1)
import Prelude (Unit, bind, ($), show)

main = launchAff $ do
  CR.runProcess $ (numberConsumer) `CR.pullFrom` numberProducer

foreign import data COUNTER :: !
type AffCounter e a = Aff (counter :: COUNTER | e) a

foreign import onCounterChange_ :: forall e.
  Fn1
  (Int -> Aff e Unit)
  (AffCounter e Unit)

onCounterChange :: forall e. (Int -> Aff e Unit) -> AffCounter e Unit
onCounterChange = runFn1 onCounterChange_

numberProducer :: forall e. CR.Producer String (Aff (avar :: AVAR, counter :: COUNTER | e)) Unit
numberProducer = CRA.produceAff (\emit -> do
  onCounterChange (\n -> do
    emit (Left $ show n)
  )
)

numberConsumer :: forall e. CR.Consumer String (Aff (console :: CONSOLE | e)) Unit
numberConsumer = forever $ do
  e <- CR.await
  lift $ log e
```

## Cause

In this particular example the error is caused by the type of `onCounterChange` and it's foreign function `onCounterChange_`.

## Fix

The callback functions for `onCounterChange` and `onCounterChange_` need to be changed from `(Int -> Aff e Unit)` to `(Int -> AffCounter e Unit)`.  Because the callback function will be run in the more restricted context of `CR.Producer` the type of the callback function needs to restricted also.
