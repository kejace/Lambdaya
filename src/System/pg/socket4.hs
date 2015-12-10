import Network.Socket
import Control.Concurrent
import Control.Monad.State.Strict
import Data.Binary.Get
import System.IO as SI
import Data.ByteString.Lazy as DBL
import Control.Monad.Trans.Either

{-
import Foreign.C.String
import Foreign.Ptr
import Data.Word
import Pipes
import Pipes.Parse as PP
import Pipes.Core as PC
import qualified Pipes.ByteString as PB
import Lens.Family.State.Strict
import Lens.Family
import Control.Monad.State.Strict
import Data.Binary.Get
import Codec.ByteString.Parser as CD
-}

import qualified Pipes.ByteString as PB
import Pipes


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
    hSetBinaryMode handle True
    runHandle handle
    return ()


runHandle handle = do 
    runEffect $ PB.hGetSome (16*1024*1024) handle >-> PB.toHandle handle

