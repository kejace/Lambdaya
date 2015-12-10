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

import System.Environment 
import System.Process

type Len = Word32
type Addr = Word32
type Reg = Word32
data MyProt = Single Addr Reg | Array Len Addr [Reg] deriving (Show)

toint (Single _ a) = fromIntegral a
single w = Single 1 w

instance Binary MyProt where
  put (Single a r) = do
        putWord32be 1
        putWord32be a
        putWord32be r

  put (Array len a r) = do
        putWord32be 2
        putWord32be len
        putWord32be a
        mapM_ putWord32be r

  get = do
    t <- getWord32be
    case t of
       1 -> 
            Single <$> getWord32be <*> getWord32be
       2 -> do
            len <- getWord32be
            addr <- getWord32be
            d <- mapM (const getWord32be) [1..len]
            return $ Array len addr d



main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    NS.listen sock 2
    mainLoop sock
 
mainLoop :: Socket -> IO ()
mainLoop sock = do
    forkIO $ outputData -- system "yes \"123456\" | head -300000 | nc localhost 4242 | wc -l" >> return ()
    conn <- NS.accept sock
    runConn conn
    close sock
    --mainLoop sock


outputData =do
    sock <- socket AF_INET Stream 0
    localhost <- inet_addr "127.0.0.1"
    NS.connect sock $ SockAddrInet 4242 localhost
    cmdlineargs <- getArgs
    let n = read $ Prelude.head $ cmdlineargs ++ ["1000000"] 
    runEffect $ each [1..n] >-> PL.map single >-> for cat encode >-> toSocket sock
    let nint = fromIntegral n 
    r <- PL.fold (+) 0 id $ runStream (fromSocket sock (64*1024) ) >->  PL.take nint >-> PL.map toint
    print r
    print "\n"
    close sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, addr) = do
    let prod =  fromSocket sock (16*1024)
    let source = runStream prod 
    let consume = for cat encode  >-> toSocket sock
    runEffect $ source  >-> consume
    close sock

--effect = for cat \x -> $ do
    --return ()


runStream = runStreamB

runStreamA producer = go producer where 
    go producer = do
      (rdata,rstream) <- runStateT decode producer
      case rdata of
        Left d -> return ()
        Right d -> do
            yield (d :: MyProt)
            go rstream


runStreamB producer = go producer where 
    go producer = do
      evalStateT parser producer where 
        parser = do
          v <- decode
          either stop repeat v where
            stop _ = return ()
            repeat a = do
                let r = a -- :: MyProt
                lift $ yield r
                parser



