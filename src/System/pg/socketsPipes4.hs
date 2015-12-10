import Network.Socket as NS
import Control.Concurrent (forkIO)
import Data.Word

import Pipes 
import qualified Pipes.Prelude as PL
import Pipes.Parse
import Pipes.ByteString

import System.IO
import Pipes.Binary 
import qualified Data.Binary as DB
import Data.Binary.Get
import Data.Binary.Put
import Pipes.Network.TCP

data MyProt = MyProt Word32 deriving Show

instance Binary MyProt where
  put (MyProt w) = putWord32be w
  get = MyProt <$> getWord32be

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    NS.listen sock 2
    mainLoop sock
 
mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- NS.accept sock
    forkIO $ runConn conn
    mainLoop sock


runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, addr) = do
    let prod =  fromSocket sock (64*1024)
    let source = runStream2 prod 
    let consume = for cat encode  >-> toSocket sock
    runEffect $ source  >-> consume
    close sock


runStream1 producer = go producer where 
    go producer = do
      (rdata,rstream) <- runStateT decode producer
      case rdata of
        Left d -> return ()
        Right d -> do
            yield (d :: MyProt)
            go rstream


runStream2 producer = go producer where 
    go producer = do
      evalStateT parser producer where 
        parser = do
          v <- decode
          case v of
            Left a -> return ()
            Right a -> do
                let r = a :: MyProt
                lift $ yield r
                parser

-- memory leaker
runStream3 producer = go producer where 
    go producer = do
      bs <- toLazyM producer
      run bs where
        run bs = do
          let a = DB.decodeOrFail bs
          case a of
            Left a -> return ()
            Right (_,_,v) -> do
                let r = v :: MyProt
                yield r
                run bs



