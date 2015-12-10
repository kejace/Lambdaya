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

import  Control.Monad as M
import Fpga2


type Len = Word32
type Addr = Word32
type Reg = Word32
data SimpleProt = Single Addr Reg | Array Len Addr [Reg] | Error deriving (Show)

toint (Single _ a) = fromIntegral a
single w = Single 1 w

simpleProtSingle = 1
simpleProtArray = 2

instance Binary SimpleProt where
  put ( Single a r) = do
        putWord32be simpleProtSingle
        putWord32be a
        putWord32be r

  put ( Array len a r) = do
        putWord32be simpleProtArray
        putWord32be len
        putWord32be a
        mapM_ putWord32be r

  put Error = return mempty

  get = do
    ty <- getWord32be
    case () 
      of _
           | ty == simpleProtSingle -> Single <$> getWord32be <*> getWord32be
           | ty == simpleProtArray  -> do
                                    len <- getWord32be
                                    addr <- getWord32be
                                    d <- M.replicateM (fromIntegral len) getWord32be
                                    return $ Array len addr d
           | otherwise -> return Error


runServer = do 
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    cmdlineargs <- getArgs
    let port = fromIntegral $ read $ Prelude.head $ cmdlineargs ++ ["4242"] 
    bindSocket sock (SockAddrInet port iNADDR_ANY)
    NS.listen sock 2
    mainLoop sock

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    NS.listen sock 2
    mainLoop sock
 
mainLoop :: Socket -> IO ()
mainLoop sock =  NS.accept sock >>= runConn >> close sock


runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, addr) = do
    let prod =  fromSocket sock (16*1024)
    let source = runStream prod 
    let consume = for cat encode  >-> toSocket sock
    runEffect $ source  >-> consume
    close sock


runStream = runStreamB

runStreamA producer = go producer where 
    go producer = do
      (rdata,rstream) <- runStateT decode producer
      case rdata of
        Left d -> return ()
        Right d -> do
            yield d
            go rstream


runStreamB producer = go producer where 
    go producer = do
      evalStateT parser producer where 
        parser = do
          v <- decode
          either stop repeat v where
            stop _ = return ()
            repeat a = do
                let r = a
                lift $ yield r
                parser


outputData =do
    sock <- socket AF_INET Stream 0
    localhost <- inet_addr "127.0.0.1"
    NS.connect sock $ SockAddrInet 4242 localhost
    cmdlineargs <- getArgs
    let n = read $ Prelude.head $ cmdlineargs ++ ["1000000"] 
    runEffect $ each [1..n] >-> PL.map single >-> for cat encode >-> toSocket sock
    let nint = fromIntegral n 
    r <- PL.fold (+) 0 id $ runStream (fromSocket sock (4*1024) ) >->  PL.take nint >-> PL.map toint
    print r
    print "\n"
    close sock
