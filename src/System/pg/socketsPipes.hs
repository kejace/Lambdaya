import Network.Socket
import Control.Concurrent (forkIO)
import Foreign.C.String
import Foreign.Ptr
import Data.Word

import Pipes 
import qualified Pipes.Prelude as PL
import Pipes.Parse as PP
import Pipes.Core as PC
import qualified Pipes.ByteString as PB
import Lens.Family.State.Strict
import Lens.Family
import Control.Monad.State.Strict
import Data.Binary.Get

import System.IO as SI
import Data.ByteString as DB
import Data.ByteString.Lazy as DBL
import Data.ByteString.Char8 as C8 (pack)

import Unsafe.Coerce


main :: IO ()
main = do
    -- create socket
    sock <- socket AF_INET Stream 0
    -- make socket immediately reusable - eases debugging.
    setSocketOption sock ReuseAddr 1
    -- listen on TCP port 4242
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    -- allow a maximum of 2 outstanding connections
    listen sock 2
    mainLoop sock
 
mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock
    forkIO $ runConn conn
    mainLoop sock


runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, addr) = do
    handle <- socketToHandle sock ReadWriteMode
    --runHandle2 handle -- >-> PL.print
    runEffect $ runLensStream parseIncoming  (PB.fromHandle handle) >-> 
        PL.map show  >-> PL.map C8.pack >-> PB.toHandle handle
    --runEffect $ PB.fromHandle handle >-> pipeMy >->
        --gather 1 >-> PL.map (DB.pack ) >-> PB.toHandle handle -- >-> PL.map C8.pack >-> PB.toHandle handle

--runHandle2 handle = do
    --loopLens parseIncoming $ PB.fromHandle handle 
        --where 
            --loopLens lens stream = do
                --hasData <- not <$> PL.null stream
                --when hasData $ do
                    --(rdata,rstream) <-  runStateT lens stream
                    ----maybe (return ()) (liftIO . print) rdata 
                    --PP.yield rdata
                    --loopLens lens rstream


-- runLensStream :: Lens -> Producer -> Producer
runLensStream lens producer = go 
  where 
    go = do
      (rdata,rstream) <- runStateT lens producer
      case rdata of
        Nothing -> return ()
        Just d -> do
            yield d
            go



--pipeMy = do
    --v <- await
    --mapM_ yield $ DB.unpack v
    --pipeMy

--pipeMy = for cat $ \x -> 
            --mapM_ yield $ DB.unpack x


gather n = do
      v <- replicateM n await
      yield v 
    ----liftIO $ print v
      gather n


nextFor par stream = do
    hasData <- not <$> PL.null stream
    when hasData $ do
        (rdata,rstream) <-  runStateT lens stream
        liftIO $ print rdata
        nextFor lens rstream

parseIncoming = do
    x <- zoom (PB.splitAt 3) draw
    y <- zoom (PB.splitAt 5) draw
    return $ sequence [x,y]
