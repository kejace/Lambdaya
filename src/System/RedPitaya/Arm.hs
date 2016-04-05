{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

-- | 
-- <http://redpitaya.com/ Red Pitaya> library for accessing FPGA from arm.
-- This code can be executed only natively on RedPitay Zinq proc using  arm ghc compiler  
--
-- check <https://github.com/ra1u/Lambdaya/blob/master/doc/build.md  doc/build.md>
-- for notes on how to compile crosscompiler  

module System.RedPitaya.Arm (
    FpgaArm,
    withOpenFpga,
)  where


#ifdef arm_HOST_ARCH

import Foreign.C.Types
import Foreign.Marshal.Array
import System.Posix.IO
import Foreign.Storable

#endif

import System.RedPitaya.Fpga
import Control.Monad.Reader 
import Foreign.Ptr


type FpgaPtr = Ptr ()
type FA a = ReaderT FpgaPtr IO a

-- | FpgaSetGet get for running on Arm
newtype FpgaArm a = FpgaArm {unFA :: FA a}
    deriving(Functor, Applicative,Monad,MonadIO,MonadReader FpgaPtr)

#ifdef arm_HOST_ARCH

runArm :: FpgaArm a -> FpgaPtr -> IO a
runArm  = runReaderT . unFA


instance FpgaSetGet FpgaArm where 
    fpgaGet o = do
        p <- getPtr
        liftIO $ peek $ plusPtr p o
    fpgaSet o reg = do
        p <- getPtr
        liftIO $ poke (plusPtr p o) reg
    fpgaGetArray o len = do
        p <- getPtr
        liftIO $ peekArray len (plusPtr p o)
    fpgaSetArray o xs = do
        p <- getPtr
        liftIO $ pokeArray (plusPtr p o) xs

getPtr :: FpgaArm FpgaPtr
getPtr = ask

-- | This function handles initialising Fpga memory mapping and
-- evaluates 'Fpga' action.
withOpenFpga :: FpgaArm () -> IO ()
withOpenFpga act = do
    fd <- openFd  "/dev/mem" ReadWrite Nothing defaultFileFlags
    setFdOption fd SynchronousWrites True
    p <- mmap nullPtr fpgaMapSize (c'PROT_READ + c'PROT_WRITE ) c'MAP_SHARED (fromIntegral fd) addrAms
    runArm act p
    munmap p fpgaMapSize
    return ()


-- | get raw pointer on fpga registry calculated from page, offset 
-- and internal state that holds memory mapped pointer
-- getOffsetPtr :: Page -> Offset -> FpgaArm (Ptr Registry)
getOffsetPtr page offset = 
    -- offset on getPtr 
    (\memmap -> plusPtr memmap (page * fpgaPageSize + offset)) <$> getPtr





fpgaMapSize = 0x100000 * 8
addrAms = 0x40000000


---------- mmap bindings

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
#else


-- | This function handles initialising Fpga memory mapping and
-- evaluates 'Fpga' action.
withOpenFpga :: FpgaArm () -> IO ()
withOpenFpga act = undefined

instance FpgaSetGet FpgaArm where 
    fpgaGet = undefined
    fpgaSet = undefined

#endif



