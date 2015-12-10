import Network.Socket
import Control.Concurrent
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

--runConn :: (Socket, SockAddr) -> IO ()
--runConn (sock, addr) = do
--    (b,len) <- newCStringLen $ foldr (++) "" $ replicate 100000 "Hi!\n"
--    runConnRaw sock (castPtr b) len
--    return ()

runConnRaw sock b len = do
    sendBuf sock b len
    runConnRaw sock b len

sendBufAll :: Socket -> Ptr Word8 -> Int -> Int -> IO Int
sendBufAll sock ptr len r
    | r < 0 = return r
    | len == 0 = return 0
    | otherwise = do
        r2 <- sendBuf sock ptr len
        sendBufAll sock (plusPtr ptr r2) (len - r2) r2


runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, addr) = do
    handle <- socketToHandle sock ReadWriteMode
    runHandle2 handle


--https://hackage.haskell.org/package/binary-0.7.6.1/docs/Data-Binary-Get.html#v:getWord32le
runHandle handle = do
    cont <- DBL.hGetContents handle
    runContent handle cont
    --print $ runGet parseDara cont
    -- return ()
    ----print d
    ----print "#"
    --hPut handle  d
    --runHandle handle

runContent handle cont = do
    let v = runGet parseDara cont
    --let d = (\x -> show x ++ "\n" ) <$> v
    --mapM_ (SI.hPutStr handle) d
     
    runContent handle cont

parseDara = do
    a <- getWord32be
    b <- getWord32be
    --liftIO $ print (a,b)
    return $ (a,b) 


runHandle2 handle = do
    loopLens parseIncoming $ PB.fromHandle handle 
        where 
            loopLens lens stream = do
                hasData <- not <$> PL.null stream
                when hasData $ do
                    (rdata,rstream) <-  runStateT lens stream
                    liftIO $ print rdata
                    loopLens lens rstream

    --runEffect $ remaining >->  PB.toHandle handle
    --runEffect $ PB.fromHandle handle >-> cat >-> pipeTest >-> PB.toHandle handle
    --runEffect $ (PB.fromHandle handle  ^. runLens) >->  PB.toHandle handle
    
nextFor lens stream = do
    hasData <- not <$> PL.null stream
    when hasData $ do
        (rdata,rstream) <-  runStateT lens stream
        liftIO $ print rdata
        nextFor lens rstream


parseIncoming = do
    x <- zoom (PB.splitAt 3) draw
    y <- zoom (PB.splitAt 5) draw
    return (x,y)
