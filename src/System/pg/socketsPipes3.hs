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

import Pipes.Binary as BIN
import Data.Binary.Get
import Data.Binary.Put

data FpgaProt = FpgaProt Word32 deriving Show

instance Binary FpgaProt where
  put (FpgaProt w) = putWord32be w
  get = do
    v <- getWord32be
    return $ FpgaProt v


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
    hSetBuffering handle $ BlockBuffering $ Just 1000000
    --runHandle2 handle -- >-> PL.print
    let prod = (PB.fromHandle handle)
    let source = runParserStream getp prod
    let consume = for cat encode  >-> PB.toHandle handle
    runEffect $ source >-> cat >-> consume
    hFlush handle

getp = do
    v <- BIN.decode 
    return (v :: Either DecodingError FpgaProt)




mapParser parser = runParserStream parser cat

runParserStream parser producer = go parser producer where 
    go parser producer = do
      (rdata,rstream) <- runStateT parser producer
      case rdata of
        Left d -> return ()
        Right d -> do
            yield d
            go parser rstream


