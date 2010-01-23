module Control.Monad.STLike.STM
    (STMS, stm, stmsToIOS,
     module Control.Monad.STLike.IO
    ) where

import Control.Concurrent.STM
import Control.Monad.STLike.IO
import Control.Monad.STLike.Unsafe

-- | Monad for STM computations with ST-like variables.
type STMS s t = STLike STM s t
instance STLikeImpl STM

-- | Lift STM computations into STMS.
stm :: STM t -> STMS s t
stm x = STLike x

-- | Run an STMS computation with /atomically/.
stmsToIOS :: STMS s t -> IOS s t
stmsToIOS x = let STLike v = x in io (atomically v)

