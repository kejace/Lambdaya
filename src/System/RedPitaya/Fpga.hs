{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.RedPitaya.Fpga (
        withOpenFpga
)
where

import Foreign.C.Types
import Foreign.Ptr
import System.Posix.IO
import Data.Int
import Foreign.Storable
import Control.Monad
import Control.Concurrent
import Control.Applicative
-- import Bindings.Posix.Sys.Mman

fpgaPageSize = 4096
fpgaMapSize = 4096 * 8
addrAms = 0x40000000

type Fpga a = IO a

withOpenFpga :: (Ptr () -> Fpga a) -> IO a
withOpenFpga act = do
    fd <- openFd  "/dev/mem" ReadWrite Nothing defaultFileFlags
    p <- mmap nullPtr fpgaMapSize (c'PROT_READ + c'PROT_WRITE ) c'MAP_SHARED (fromIntegral fd) addrAms
    r <- act p
    munmap p fpgaMapSize
    return r


testled:: Ptr () -> Fpga ()
testled ptr = do
    let pled = plusPtr ptr 0x30  :: Ptr Int32
    forM  [0,2..256] $ \n -> do
        threadDelay 10000
        poke pled $ fromIntegral n
    print "done"
    return ()


fpgaPtr :: Int -> Int -> Ptr  Int32 -> Ptr Int32
fpgaPtr page offset mem = plusPtr mem (page * fpgaPageSize + offset)


fpgaGet :: (Int32 -> a) -> Ptr Int32  -> Fpga a
fpgaGet f p = f <$> (peek p) 

fpgaSet ::  Ptr Int32 -> (a -> Int32 ) -> a -> Fpga ()
fpgaSet p f v = poke p $ f v

fpgaId :: Ptr Int32 -> Fpga Int32
fpgaId p = fpgaGet id ( fpgaPtr 0 0 p)

dna1 :: Ptr Int32 -> Fpga Int32
dna1 p = fpgaGet id ( fpgaPtr 0 0x4 p)

dna2 :: Ptr Int32 -> Fpga Int32
dna2 p = fpgaGet id ( fpgaPtr 0 0x4 p)


foreign import ccall "mmap" mmap
  :: Ptr () -> CSize -> CInt -> CInt-> CInt-> CInt -> IO (Ptr ())

foreign import ccall "munmap" munmap
  :: Ptr () -> CSize -> IO CInt


c'PROT_EXEC = 4
c'PROT_NONE = 0
c'PROT_READ = 1
c'PROT_WRITE = 2
c'MAP_FIXED = 16
c'MAP_PRIVATE = 2
c'MAP_SHARED = 1
c'MAP_FAILED = wordPtrToPtr 4294967295

--mmap = c'mmap
--munmap = c'munmap


-- main = withOpenFpga 
