Here is simple example of code that triggers this error...

```purescript
module Main where

import Prelude
import Data.Monoid
import Control.Monad.Eff
import Graphics.Canvas

main = do          
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  setFillStyle "#00ff00" ctx
  fillPath ctx $ rect ctx
    { x: 0.0
    , y: 0.0
    , w: 300.0
    , h: 100.0
    }
```

Which will generate the error...

```
Unknown data constructor Just
```

Because `Just` is defined in the Data.Maybe package and Data.Maybe package has not been imported.
