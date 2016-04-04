{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


-- | Implemention of both client and server for contolling fpga over tcp
module System.RedPitaya.Tcp (
   NetworkFpgaSetGet(..),
   runRemoteRp ,
   runRpServer
) where

import System.RedPitaya.Fpga
import System.RedPitaya.Arm

import Network.Socket as NS
import Control.Concurrent (forkIO)
import Data.Word

import Pipes as P
import qualified Pipes.Prelude as PP
import Pipes.Parse

import Pipes.Binary 
import Data.Binary.Get
import Data.Binary.Put
import Pipes.Network.TCP


type Len = Word32
type Addr = Word32
type Reg = Word32
data SimpleProt = WriteSingle Addr Reg 
                | WriteArray Len Addr [Reg] 
                | ReadSingle Addr
                | ReadArray Len Addr
                | RespSingle Reg
                | RespArray Len [Reg]
                | Error deriving (Show,Eq)



toint (WriteSingle _ a) = fromIntegral a
single w = WriteSingle 1 w

cWriteSingle = 1
cWriteArray = 2
cReadSingle = 3
cReadArray = 4
cRespSingle = 5
cRespArray = 6
cError = 7

instance Binary SimpleProt where
  put ( WriteSingle a r) =  putWord32be cWriteSingle
                            >> putWord32be a
                            >> putWord32be r

  put ( WriteArray len a xs) = putWord32be cWriteArray
                              >> putWord32be len
                              >> putWord32be a
                              >> mapM_ putWord32be xs

  put (ReadSingle a) = putWord32be cReadSingle
                       >> putWord32be a

  put (ReadArray len addr) = putWord32be cReadArray 
                             >> putWord32be addr

  put (RespSingle reg) = putWord32be cRespSingle >> putWord32be reg

  put (RespArray len arr) = putWord32be cRespArray
                            >> putWord32be len
                            >> mapM_ putWord32be arr

  put Error = putWord32be cError

  get = do
    ty <- getWord32be
    case () 
      of _
           | ty == cWriteSingle -> WriteSingle <$> getWord32be <*> getWord32be
           | ty == cWriteArray  -> do
                                    len <- getWord32be
                                    WriteArray len <$> getWord32be <*> parseArray len 
           | ty == cReadSingle  -> ReadSingle <$> getWord32be
           | ty == cReadArray   -> ReadArray <$> getWord32be <*> getWord32be
           | ty == cRespSingle  -> RespSingle <$> getWord32be
           | ty == cRespArray   -> do 
                                      len <- getWord32be
                                      RespArray len <$> parseArray len 
           | otherwise -> return Error

parseArray len = sequenceA ( replicate ( fromIntegral len) getWord32be )

-- | This can be used to implement run server on RedPitaya
runRpServer:: PortNumber -> IO ()
runRpServer port = do 
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet port iNADDR_ANY)
    NS.listen sock 2
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop s =  go where
    go = do
        (sock, addr) <- NS.accept s
        setSocketOption sock NoDelay 1
        let rx = fromSocket sock (4*1024)
        let tx = toSocket sock
        forkIO $ runConn (rx,tx)
        go

runConn (rx,tx) = withOpenFpga $ runEffect $ 
                    runStream rx  >-> P.for cat processPacket >-> PP.takeWhile (/= Error) >-> P.for cat encode  >-> tx

frInt :: (Integral a, Num b) => a -> b
frInt = fromIntegral

processPacket =  handle
  where 
    handle (WriteSingle addr reg) = lift $ ( fpgaSet (frInt addr) reg )
    handle (WriteArray len addr arr) = lift $ fpgaSetArray (frInt addr) arr
    handle (ReadSingle addr ) =   lift (fpgaGet (frInt addr)) >>= (yield . RespSingle )
    handle (ReadArray len addr ) =  lift (fpgaGetArray (frInt addr) (frInt len)) 
                                             >>= yield . RespArray len
    handle _ = yield Error


main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    NS.listen sock 2
    mainLoop sock
 

runStream :: (Monad m, Binary b) =>
     Producer ByteString (Proxy x x' () b m) r
     -> Proxy x x' () b m ()


runStream producer = go producer where 
    go producer = do
      evalStateT parser producer where 
        parser = do
          v <- decode
          either stop repeat v where
            stop _ = return ()
            repeat a = do
                lift $ yield a
                parser



--- Pc side
type FpgaProtocol = SimpleProt

-- | Type that implements FpgaSetGet over network
type NetworkFpgaSetGet = Pipe FpgaProtocol FpgaProtocol IO

-- | This evaluates FpgaSetGet action that sends commands on RedPitaya
runRemoteRp :: HostName -> PortNumber -> NetworkFpgaSetGet () -> IO ()
runRemoteRp addr port act = do
    sock <- socket AF_INET Stream 0
    host <- inet_addr addr
    NS.connect sock $ SockAddrInet port host
    setSocketOption sock NoDelay 1
    runEffect $ runStream (fromSocket sock (4*1024)) >-> act >-> P.for cat encode >-> toSocket sock
    close sock
    return ()

instance FpgaSetGet NetworkFpgaSetGet where 
    fpgaGet offset = do
        yield (ReadSingle $ frInt offset)
        await >>= respsing
          where
           respsing (RespSingle reg) = return reg
           respsing _ = yield Error >> return 0

    fpgaSet off reg = yield  $ WriteSingle (frInt off) reg

    fpgaGetArray offset len = do 
        yield $ ReadArray ( frInt len) (frInt offset)
        await >>= resparray
          where
            resparray (RespArray _ xs) = return xs
            resparray _ = yield Error >> return []

    fpgaSetArray offset xs = yield $ WriteArray ( frInt (length xs)) (frInt offset) xs


