-- import Control.Monad.Trans.Resource

import Data.Conduit
import qualified Data.Conduit.List as CL

sourceList :: Monad m => [o] -> Source m o
sourceList = ???

main = sourceList [1, 2, 3] $$ CL.mapM_ print
