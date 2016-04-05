{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | 
-- <http://redpitaya.com/ Red Pitaya> library for accessing FPGA from arm.
-- This code can be executed only natively on RedPitay Zinq proc using  arm ghc compiler  
--
-- check <https://github.com/ra1u/Lambdaya/blob/master/doc/build.md  doc/build.md>
-- for notes on how to compile crosscompiler  

module System.RedPitaya.Debug (
    FpgaDebug,
    withDebugFpga,
)  where

import System.RedPitaya.Fpga
import Control.Monad.State.Lazy

type FpgaDebugState = Registry

type FD a = StateT FpgaDebugState IO a

newtype FpgaDebug a = FpgaDebug { unDebug :: FD a }
    deriving(Functor,Applicative,Monad,MonadFix,MonadIO,MonadState FpgaDebugState)

runDebug :: FpgaDebug a -> FpgaDebugState -> IO (a,FpgaDebugState)
runDebug  = runStateT . unDebug


instance FpgaSetGet FpgaDebug where 
    fpgaGet o = do
        get
    fpgaSet o reg = do
        put reg
    fpgaGetArray o len = do
        p <- get
        return $ take len $ iterate (+1) p
    fpgaSetArray o xs = do
        put $ head $ xs ++ [0]
        return ()


-- | Run debug .
withDebugFpga :: FpgaDebug () -> IO ()
withDebugFpga act = do
    runDebug act 0
    return ()



