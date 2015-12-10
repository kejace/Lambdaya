
    type Offset = Int
    type Page = Int
    type Registry = Word32

    type FpgaMonad a = StateT (Ptr ()) IO  a

    newtype Fpga a = Fpga (FpgaMonad a)
        deriving (Functor,Applicative, Monad,MonadIO, MonadFix, MonadPlus, Alternative)

    withOpenFpga :: Fpga a -> IO a

    fpgaRead :: Page -> Offset -> Fpga Registry
    fpgaWrite :: Page ->  Offset -> Registry -> Fpga ()

    -- bunch of function on top of fpgaRead/fpgaWrite
    setSomReg :: Registry -> Fpga ()
    getSomReg :: Fpga Registry

Is current approach to write and read registries on embedded device. So far so good and now I have idea, to add new functionality that will enable calling `setSomReg` / `getSomReg` over the network.
Goal is to replace  direct [`peek`](http://hackage.haskell.org/package/base-4.8.1.0/docs/Foreign-Storable.html#v:peek) and [`poke`](http://hackage.haskell.org/package/base-4.8.1.0/docs/Foreign-Storable.html#v:poke) with some other monad.

I would like to keep same  signatures for most get/set functions (so that same code can be run either way) and my question is what approach should be taken orat least that minimal change is requred.

So far I was able to test rewrite to something like this, but requres verbose type annotination. 

    -- this works fine
    monadicIncr :: (Integral a , Monad m ) => a -> m a
    monadicIncr a = return (a + 1)

    -- here GHC complains about ambigous type signature if not provided
    m2 = monadicInc

Here is full source: https://hackage.haskell.org/package/Lambdaya-0.1.1.0/docs/src/System-RedPitaya-Fpga.html
(no network code yet). To be clear I have question about how to desing types around 
approach on supporting both direct and over the wire  
