{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | 
-- <http://redpitaya.com/ Red Pitaya> native library for accessing  <https://github.com/RedPitaya/RedPitaya/blob/master/FPGA/release1/doc/RedPitaya_HDL_memory_map.odt?raw=true Fpga memory>
module System.RedPitaya.Fpga (
    Fpga,
    Registry,
    withOpenFpga,
    -- * Housekeeping
    -- | various housekeeping and Gpio functions
    fpgaId,
    dna,
    setExpDirP,
    getExpDirP,
    setExpDirN,
    getExpDirN,
    setExpOutP,
    setExpOutN,
    getExpInP,
    getExpInN,
    -- single pin functions
    GpioType(..),
    GpioDirection(..),
    PinNum,
    setExpDir,
    GpioValue(..),
    setExpOut,
    getExpOut,

    setLed,
    -- * Oscilloscope
    -- | functions for accessing oscilloscope features
    resetWriteSM,
    triggerNow,
    TriggerSource(..),
    setOscTrigger,
    triggerDelayEnded,
    setChATreshold,
    getChATreshold,
    setChBTreshold,
    getChBTreshold,
    setDelayAfterTrigger,
    getDelayAfterTrigger,
    setOscDecimationRaw,
    getOscDecimationRaw,
    OscDecimation(..),
    setOscDecimation,
    getOscWpCurrent,
    getOscWpTrigger,
    getOscChAHysteresis,
    setOscChAHysteresis,
    getOscChBHysteresis,
    setOscChBHysteresis,
    enableOscDecimationAvarage,
    setChAEqualFilter,
    getChAEqualFilter,
    setChBEqualFilter,
    getChBEqualFilter,
    setChAAxiLowerAddress,
    getChAAxiLowerAddress,
    setChAAxiUpperAddress,
    getChAAxiUpperAddress,
    getChAAxiDelayAfterTrigger,
    setChAAxiDelayAfterTrigger,
    enableChAAxiMaster,
    getChAAxiWritePtrTrigger,
    getChAAxiWritePtrCurrent,
    setChBAxiLowerAddress,
    getChBAxiLowerAddress,
    setChBAxiUpperAddress,
    getChBAxiUpperAddress,
    getChBAxiDelayAfterTrigger,
    setChBAxiDelayAfterTrigger,
    enableChBAxiMaster,
    getChBAxiWritePtrTrigger,
    getChBAxiWritePtrCurrent,
    getOscChABuffer,
    getOscChBBuffer,
    -- * Plumbing
    -- | low level functions for direct Fpga access, used to extend interface
    Page,
    Offset,
    fpgaRead,
    fpgaWrite,
    pokeFpgaArray,
    peekFpgaArray
)   
where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import System.Posix.IO
import Data.Int
import Data.Word
import Data.Bits
import Foreign.Storable
import Control.Monad
import Control.Concurrent
import Control.Applicative

import Control.Monad.State  
import Control.Applicative
import Control.Monad.Fix
import Control.Monad
-- import Bindings.Posix.Sys.Mman



fpgaPageSize = 4096
fpgaMapSize = 4096 * 8
addrAms = 0x40000000

type FpgaPtr = Ptr ()
type FpgaState = FpgaPtr

-- | type representing fpga memory offset from page
type Offset = Int
-- | type representing fpga memory page
type Page = Int
-- | type representing fpga registry
type Registry = Word32
-- | Fpga monad is hidden behind this type
type StateMonad a = StateT FpgaState IO  a

-- | Environment where one can read and write Fpga registries
newtype Fpga a = Fpga (StateMonad a) 
    deriving (Functor,Applicative, Monad,MonadIO, MonadFix, MonadPlus, Alternative)


getStateType :: Fpga a -> StateMonad a
getStateType (Fpga s) = s 

-- Constructor
fpgaState a = Fpga ( StateT a )

runFpga :: Fpga a -> FpgaState -> IO (a,FpgaState) 
runFpga (Fpga s) = runStateT s

store ::  FpgaState -> Fpga ()
store v = fpgaState $ \x -> ( return ((),v) )

getState :: Fpga FpgaState
getState = fpgaState $  \x -> ( return  (x,x) )

getPtr :: Fpga FpgaPtr
getPtr = getState


-- | This function handles initialising Fpga memory mapping and
-- evaluates 'Fpga' action.
withOpenFpga :: Fpga a -> IO a
withOpenFpga act = do
    fd <- openFd  "/dev/mem" ReadWrite Nothing defaultFileFlags
    setFdOption fd SynchronousWrites True
    p <- mmap nullPtr fpgaMapSize (c'PROT_READ + c'PROT_WRITE ) c'MAP_SHARED (fromIntegral fd) addrAms
    (r,s) <- runFpga act p
    munmap p fpgaMapSize
    return r

-- | get raw pointer on fpga registry calculated from page, offset 
-- and internal state that holds memory mapped pointer
getOffsetPtr :: Page -> Offset -> Fpga (Ptr Registry)
getOffsetPtr page offset = 
    -- offset on getPtr 
    (\memmap -> plusPtr memmap (page * fpgaPageSize + offset)) <$> getPtr

-- | direct read from fpga registry
fpgaRead :: Page -> Offset -> Fpga Registry
fpgaRead page offset = do
    p <- getOffsetPtr page offset
    liftIO $ peek p

-- | direct write in fpga registry
fpgaWrite :: Page ->  Offset -> Registry -> Fpga ()
fpgaWrite page offset reg = do
    p <- getOffsetPtr page offset
    liftIO $ poke p reg

-- | write array in fpga memory
pokeFpgaArray :: Page -> Offset -> [Registry] -> Fpga ()
pokeFpgaArray page offset xs =  do
    p <- getOffsetPtr page offset
    liftIO $ pokeArray p xs

-- | read array from fpga memory, passing page, offset and length
peekFpgaArray :: Page -> Offset -> Int -> Fpga [Registry]
peekFpgaArray page offset len = do
    p <- getOffsetPtr page offset
    liftIO $ peekArray len p


---------------------------------------------------------
-- * Housekeeping memory map

-- | get ID ,  0 prototype , 1 release
fpgaId :: Fpga Registry
fpgaId = fpgaRead 0 0

-- | get DNA
dna :: Fpga Integer
dna = do
    dna1 <- fromIntegral <$> fpgaRead 0 4
    dna2 <- fromIntegral <$> fpgaRead 0 8
    return $ dna1 + (2^32)*dna2

-- | set expansion connector direction P registry
--
-- 1 out , 0 in  
setExpDirP :: Registry -> Fpga ()
setExpDirP = fpgaWrite 0 0x10

-- | get expansion connector direction P registry
--
-- 1 out , 0 in 
getExpDirP :: Fpga Registry
getExpDirP = fpgaRead 0 0x10

-- | set expansion connector direction N registry
--
-- 1 out , 0 in 
setExpDirN :: Registry -> Fpga ()
setExpDirN = fpgaWrite 0 0x14

-- | get expansion connector direction N registry
--
-- 1 out , 0 in 
getExpDirN :: Fpga Registry
getExpDirN = fpgaRead 0 0x14

-- | expansion connector  P output registry value
setExpOutP :: Registry -> Fpga ()
setExpOutP = fpgaWrite 0 0x18

-- | expansion connector  P output registry value
getExpOutP :: Fpga Registry
getExpOutP = fpgaRead 0 0x18

-- | expansion connector  N output registry value
setExpOutN :: Registry -> Fpga ()
setExpOutN = fpgaWrite 0 0x1C

-- | expansion connector  N output registry value
getExpOutN :: Fpga Registry
getExpOutN = fpgaRead 0 0x1C

-- | expansion connector  P input registry value
getExpInP :: Fpga Registry
getExpInP =  fpgaRead 0 0x20

-- | expansion connector  N input registry value
getExpInN :: Fpga Registry
getExpInN =  fpgaRead 0 0x24

-- | type of gpio can be either P on N
data GpioType =
    -- | P gpio
    P | 
    -- | N gpio
    N
    deriving (Show)

class ToBool b where 
    toBool :: b -> Bool 

setBitValue :: (Bits a,ToBool b) => b -> Int  -> a -> a
setBitValue b
    | toBool b = flip setBit
    | otherwise = flip clearBit

-- | represent gpio direction, that can be either Input or Output
data GpioDirection =
    Input | 
    Output
    deriving (Show)

instance ToBool GpioDirection where
    toBool Input = True
    toBool Output = False


-- | type representing pin number
type PinNum = Int

-- | Sets direction of pin
setExpDir :: GpioType -> GpioDirection -> PinNum ->  Fpga ()
setExpDir N d p = setBitValue d p <$> getExpDirN  >>= setExpDirN
setExpDir P d p = setBitValue d p <$> getExpDirP  >>= setExpDirP


-- | represent gpio value that can be either Hi or Low 
data GpioValue =
    Low | 
    Hi
    deriving (Show)

instance ToBool GpioValue where
    toBool Low = False
    toBool Hi = True

-- | Sets outout value of pin
setExpOut :: GpioType -> GpioValue -> PinNum ->  Fpga ()
-- read using getExpOutN , fmap over setBitValue and bind in setExpOutX
setExpOut N v p = setBitValue v p <$> getExpOutN  >>= setExpOutN
setExpOut P v p = setBitValue v p <$> getExpOutP  >>= setExpOutP


toGpioValue :: Bool -> GpioValue
toGpioValue True = Hi
toGpioValue False = Low

-- | Sets output value of single pin
getExpOut :: GpioType  -> PinNum ->  Fpga GpioValue
getExpOut N p = (\x -> toGpioValue ( testBit x p )) <$> getExpOutN
getExpOut P p = (\x -> toGpioValue ( testBit x p )) <$> getExpOutP

-- | write in led registry
setLed :: Registry -> Fpga ()
setLed = fpgaWrite 0 0x30

---------------------------------------
-- * Oscilloscope

osciloscpeFpgaPage :: Int
osciloscpeFpgaPage = 1

fpgaWriteOsc = fpgaWrite osciloscpeFpgaPage

fpgaReadOsc = fpgaRead osciloscpeFpgaPage


-- | reset write state machine for oscilloscope
resetWriteSM :: Fpga ()
resetWriteSM = fpgaWriteOsc 0 3

-- | start writing data into memory (ARM trigger).
triggerNow :: Fpga ()
triggerNow = fpgaWriteOsc 0 1

-- | oscilloscope trigger selection
data TriggerSource = 
    -- | trig immediately
    Immediately  
    -- | ch A threshold positive edge
    | ChAPositiveEdge 
    -- | ch A threshold negative edge
    | ChANegativeEdge 
    -- | ch B threshold positive edge
    | ChBPositiveEdge 
    -- | ch B threshold negative edge
    | ChBNegativeEdge 
    -- | external trigger positive edge - DIO0_P pin
    | ExtPositiveEdge 
    -- | external trigger negative edge
    | ExtNegaitveEdge 
    -- | arbitrary wave generator application positive edge
    | AWGPositiveEdge 
    -- | arbitrary wave generator application negative edge
    | AWGNegativeEdge 
    deriving (Show)

-- | set oscilloscope trigger
setOscTrigger :: TriggerSource -> Fpga ()
setOscTrigger Immediately     = setOscTriggerHelper 1
setOscTrigger ChAPositiveEdge = setOscTriggerHelper 2
setOscTrigger ChANegativeEdge = setOscTriggerHelper 3
setOscTrigger ChBPositiveEdge = setOscTriggerHelper 4
setOscTrigger ChBNegativeEdge = setOscTriggerHelper 5
setOscTrigger ExtPositiveEdge = setOscTriggerHelper 6
setOscTrigger ExtNegaitveEdge = setOscTriggerHelper 7
setOscTrigger AWGPositiveEdge = setOscTriggerHelper 8
setOscTrigger AWGNegativeEdge = setOscTriggerHelper 9


setOscTriggerHelper :: Registry -> Fpga ()
setOscTriggerHelper = fpgaWriteOsc 0x4


-- | when trigger delay is value becomes 'True'
triggerDelayEnded :: Fpga Bool
triggerDelayEnded = (==0) <$> fpgaReadOsc 0x4

-- | Ch A threshold, makes trigger when ADC value cross this value
setChATreshold :: Registry -> Fpga ()
setChATreshold = fpgaWriteOsc 0x8

-- | gets cha A threshold
getChATreshold :: Fpga Registry
getChATreshold = fpgaReadOsc 0x8

-- | Ch B threshold, makes trigger when ADC value cross this value
setChBTreshold :: Registry -> Fpga ()
setChBTreshold = fpgaWriteOsc 0xc

-- | gets ch B threshold
getChBTreshold :: Fpga Registry
getChBTreshold = fpgaReadOsc 0xc

-- | Number of decimated data after trigger written into memory
setDelayAfterTrigger :: Registry -> Fpga ()
setDelayAfterTrigger = fpgaWriteOsc 0x10

-- | gets delay after trigger value
getDelayAfterTrigger :: Fpga Registry
getDelayAfterTrigger = fpgaReadOsc 0x10

-- | sets oscilloscope decimation registry, allows only
-- 1,8, 64,1024,8192,65536. If other value is written data will NOT be correct.
setOscDecimationRaw :: Registry -> Fpga ()
setOscDecimationRaw =  fpgaWriteOsc 0x14

-- | oscilloscope decimation registry value
getOscDecimationRaw :: Fpga Registry
getOscDecimationRaw =  fpgaReadOsc 0x14

-- | oscilloscope decimation
data OscDecimation = 
    -- | 1
    OscDec1  
    -- | 8
    | OscDec8
    -- | 64
    | OscDec64 
    -- | 1024 
    | OscDec1024
    -- | 8192
    | OscDec8192
    -- | 65536
    | OscDec65536
    deriving (Show)

-- | set oscilloscope decimation
setOscDecimation :: OscDecimation -> Fpga ()
setOscDecimation OscDec1 = setOscDecimationRaw 1
setOscDecimation OscDec8 = setOscDecimationRaw 8
setOscDecimation OscDec64 = setOscDecimationRaw 64
setOscDecimation OscDec1024 = setOscDecimationRaw 1024
setOscDecimation OscDec8192 = setOscDecimationRaw 8192
setOscDecimation OscDec65536 = setOscDecimationRaw 65536

-- | write pointer - current
getOscWpCurrent:: Fpga Registry
getOscWpCurrent = fpgaReadOsc 0x18

-- | write pointer - trigger
getOscWpTrigger :: Fpga Registry
getOscWpTrigger = fpgaReadOsc 0x1C

-- | Ch A hysteresis
getOscChAHysteresis :: Fpga Registry
getOscChAHysteresis = fpgaReadOsc 0x20

-- | set Ch A hysteresis
setOscChAHysteresis :: Registry -> Fpga ()
setOscChAHysteresis = fpgaWriteOsc 0x20

-- | Ch B hysteresis
getOscChBHysteresis :: Fpga Registry
getOscChBHysteresis = fpgaReadOsc 0x24

-- | set Ch B hysteresis
setOscChBHysteresis :: Registry -> Fpga ()
setOscChBHysteresis = fpgaWriteOsc 0x24

-- | Enable signal average at decimation True enables, False disables
enableOscDecimationAvarage :: Bool -> Fpga ()
enableOscDecimationAvarage True = fpgaWriteOsc 0x28 1
enableOscDecimationAvarage False = fpgaWriteOsc 0x28 0

-- | set ch A equalization filter, takes array with coefficients [AA,BB,KK,PP]
setChAEqualFilter :: [Registry] -> Fpga ()
setChAEqualFilter = pokeFpgaArray osciloscpeFpgaPage 0x30 . take 4

-- | get ch A equalization filter, return array with coefficients [AA,BB,KK,PP]
getChAEqualFilter :: Fpga [Registry]
getChAEqualFilter = peekFpgaArray osciloscpeFpgaPage 0x30 4

-- | set ch B equalization filter, takes array with coefficients [AA,BB,KK,PP]
setChBEqualFilter :: [Registry] -> Fpga ()
setChBEqualFilter = pokeFpgaArray osciloscpeFpgaPage 0x40 . take 4

-- | get ch A equalization filter, return array with coefficients [AA,BB,KK,PP]
getChBEqualFilter :: Fpga [Registry]
getChBEqualFilter = peekFpgaArray osciloscpeFpgaPage 0x40 4

-- | starting writing address ch A - CH A AXI lower address
setChAAxiLowerAddress :: Registry -> Fpga ()
setChAAxiLowerAddress = fpgaWriteOsc 0x50

-- | read - starting writing address ch A - CH A AXI lower address
getChAAxiLowerAddress :: Fpga Registry
getChAAxiLowerAddress = fpgaReadOsc 0x50

-- | starting writing address ch A - CH A AXI lower address
setChAAxiUpperAddress :: Registry -> Fpga ()
setChAAxiUpperAddress = fpgaWriteOsc 0x54

-- | read - starting writing address ch A - CH A AXI lower address
getChAAxiUpperAddress :: Fpga Registry
getChAAxiUpperAddress = fpgaReadOsc 0x54

-- | read - Number of decimated data after trigger written into memory
getChAAxiDelayAfterTrigger :: Fpga Registry
getChAAxiDelayAfterTrigger = fpgaReadOsc 0x58

-- | set umber of decimated data after trigger written into memory
setChAAxiDelayAfterTrigger :: Registry -> Fpga ()
setChAAxiDelayAfterTrigger = fpgaWriteOsc 0x58

-- | Enable AXI master
enableChAAxiMaster :: Bool -> Fpga ()
enableChAAxiMaster True = fpgaWriteOsc 0x5c 1
enableChAAxiMaster False = fpgaWriteOsc 0x5c 0

-- | Write pointer for ch A at time when trigger arrived
getChAAxiWritePtrTrigger :: Fpga Registry
getChAAxiWritePtrTrigger = fpgaReadOsc 0x60

-- | current write pointer for ch A
getChAAxiWritePtrCurrent :: Fpga Registry
getChAAxiWritePtrCurrent = fpgaReadOsc 0x64

------------------------------------------------

-- | starting writing address ch B - CH B AXI lower address
setChBAxiLowerAddress :: Registry -> Fpga ()
setChBAxiLowerAddress = fpgaWriteOsc 0x70

-- | read - starting writing address ch B - CH B AXI lower address
getChBAxiLowerAddress :: Fpga Registry
getChBAxiLowerAddress = fpgaReadOsc 0x70

-- | starting writing address ch B - CH B AXI lower address
setChBAxiUpperAddress :: Registry -> Fpga ()
setChBAxiUpperAddress = fpgaWriteOsc 0x74

-- | read - starting writing address ch B - CH B AXI lower address
getChBAxiUpperAddress :: Fpga Registry
getChBAxiUpperAddress = fpgaReadOsc 0x74

-- | read - Number of decimated data after trigger written into memory
getChBAxiDelayAfterTrigger :: Fpga Registry
getChBAxiDelayAfterTrigger = fpgaReadOsc 0x78

-- | set umber of decimated data after trigger written into memory
setChBAxiDelayAfterTrigger :: Registry -> Fpga ()
setChBAxiDelayAfterTrigger = fpgaWriteOsc 0x78

-- | Enable AXI master B
enableChBAxiMaster :: Bool -> Fpga ()
enableChBAxiMaster True = fpgaWriteOsc 0x7c 1
enableChBAxiMaster False = fpgaWriteOsc 0x7c 0

-- | Write pointer for ch B at time when trigger arrived
getChBAxiWritePtrTrigger :: Fpga Registry
getChBAxiWritePtrTrigger = fpgaReadOsc 0x80

-- | current write pointer for ch B
getChBAxiWritePtrCurrent :: Fpga Registry
getChBAxiWritePtrCurrent = fpgaReadOsc 0x84


-- | reads oscilloscope buffer for channel A from Fpga passing offset and length. 
-- buffer should fit within 16k sampling range.
-- Returns  less than requested data if trying to read over the bounds.
getOscChABuffer :: Offset -> Int -> Fpga [Registry]
getOscChABuffer off len = peekFpgaArray osciloscpeFpgaPage (off' + 0x10000) len'
                          where
                            off' = max 0 off
                            len' = min (0x10000 - off) len

-- | reads oscilloscope buffer for channel B from Fpga passing offset and length. 
-- buffer should fit within 16k sampling range.
-- Returns less than requested data if trying to read over the bounds.
getOscChBBuffer :: Offset -> Int -> Fpga [Registry]
getOscChBBuffer off len = peekFpgaArray osciloscpeFpgaPage (off' + 0x20000) len'
                          where
                            off' = max 0 off 
                            len' = min (0x10000 - off) len

--------------------------------------------------------------------


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


-- main = print "hello World" --withOpenFpga testled
