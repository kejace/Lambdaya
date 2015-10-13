{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | 
-- <http://redpitaya.com/ Red Pitaya> native library for accessing Fpga

module System.RedPitaya.Fpga (
    Fpga,
    Registry,
    Channel,
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
    getLed,
    -- * Oscilloscope
    -- | functions for accessing oscilloscope features
    resetWriteSM,
    triggerNow,
    TriggerSource(..),
    setOscTrigger,
    triggerDelayEnded,
    setTreshold,
    getTreshold,
    setDelayAfterTrigger,
    getDelayAfterTrigger,
    setOscDecimationRaw,
    getOscDecimationRaw,
    OscDecimation(..),
    setOscDecimation,
    getOscWpCurrent,
    getOscWpTrigger,
    getOscHysteresis,
    setOscHysteresis,
    enableOscDecimationAvarage,
    setEqualFilter,
    getEqualFilter,
    setAxiLowerAddress,
    getAxiLowerAddress,
    setAxiUpperAddress,
    getAxiUpperAddress,
    setAxiDelayAfterTrigger,
    getAxiDelayAfterTrigger,
    enableAxiMaster,
    getAxiWritePtrTrigger,
    getAxiWritePtrCurrent,
    getOscBuffer,
    -- * Arbitrary Signal Generator (ASG)
    getAsgOption,
    setAsgOption,
    setAsgOptionBExtGatRep,
    getAsgOptionBExtGatRep,
    setAsgAmplitudeScale,
    setAsgAmplitudeOffset,
    setAsgCounterWrap,
    setAsgCounterStartOffset,
    setAsgCounterStep,
    getAsgCounterReadPtr,
    setAsgCounterReadPtr,
    getAsgNumReadCycles,
    setAsgNumReadCycles,
    getAsgNumRepetitions,
    setAsgNumRepetitions,
    getAsgBurstDelay,
    setAsgBurstDelay,
    -- * Plumbing
    -- | low level functions for direct Fpga access, used to extend interface
    Page,
    Offset,
    fpgaRead,
    fpgaWrite,
    fpgaFmap,
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
import Control.Applicative
import Control.Monad.State  



fpgaPageSize = 0x100000
fpgaMapSize = 0x100000 * 8
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

-- | Redpitaya Channel A or B.
data Channel = A | B

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

-- | apply transformation on fpga registry value 
fpgaFmap :: Page ->  Offset -> (Registry -> Registry) -> Fpga ()
fpgaFmap page offset f = do
    reg <- fpgaRead page offset
    fpgaWrite page offset (f reg)

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

-- | read in led registry
getLed :: Fpga Registry
getLed = fpgaRead 0 0x30

---------------------------------------
-- * Oscilloscope

osciloscpeFpgaPage :: Int
osciloscpeFpgaPage = 1

fpgaWriteOsc = fpgaWrite osciloscpeFpgaPage
fpgaReadOsc = fpgaRead osciloscpeFpgaPage


-- | reset write state machine for oscilloscope
resetWriteSM :: Fpga ()
resetWriteSM = fpgaWriteOsc 0 2

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

-- | Ch x threshold, makes trigger when ADC value cross this value
setTreshold :: Channel -> Registry -> Fpga ()
setTreshold A = fpgaWriteOsc 0x8
setTreshold B = fpgaWriteOsc 0xc

-- | gets ch x threshold
getTreshold :: Channel -> Fpga Registry
getTreshold A = fpgaReadOsc 0x8
getTreshold B = fpgaReadOsc 0xc

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
    OscDec1  
    | OscDec8
    | OscDec64 
    | OscDec1024
    | OscDec8192
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

-- | ch x hysteresis
getOscHysteresis :: Channel -> Fpga Registry
getOscHysteresis A = fpgaReadOsc 0x20
getOscHysteresis B = fpgaReadOsc 0x24

-- | set ch x hysteresis
setOscHysteresis :: Channel -> Registry -> Fpga ()
setOscHysteresis A = fpgaWriteOsc 0x20
setOscHysteresis B = fpgaWriteOsc 0x24

-- | Enable signal average at decimation True enables, False disables
enableOscDecimationAvarage :: Bool -> Fpga ()
enableOscDecimationAvarage True = fpgaWriteOsc 0x28 1
enableOscDecimationAvarage False = fpgaWriteOsc 0x28 0

-- | set ch A equalization filter, takes array with coefficients [AA,BB,KK,PP]
setEqualFilter :: Channel -> [Registry] -> Fpga ()
setEqualFilter A = pokeFpgaArray osciloscpeFpgaPage 0x30 . take 4
setEqualFilter B = pokeFpgaArray osciloscpeFpgaPage 0x40 . take 4

-- | get ch x equalization filter, return array with coefficients [AA,BB,KK,PP]
getEqualFilter :: Channel -> Fpga [Registry]
getEqualFilter A = peekFpgaArray osciloscpeFpgaPage 0x30 4
getEqualFilter B = peekFpgaArray osciloscpeFpgaPage 0x40 4


setAxiGeneric' :: Offset -> Channel -> Registry -> Fpga ()
setAxiGeneric' offest A = fpgaWriteOsc offest
setAxiGeneric' offest B = fpgaWriteOsc (offest+0x20)

getAxiGeneric' :: Offset -> Channel -> Fpga Registry
getAxiGeneric' offest A = fpgaReadOsc offest
getAxiGeneric' offest B = fpgaReadOsc (offest+0x20)


-- | starting writing address ch x - CH x AXI lower address
setAxiLowerAddress :: Channel -> Registry -> Fpga ()
setAxiLowerAddress = setAxiGeneric' 0x50

-- | read - starting writing address ch x - CH x AXI lower address
getAxiLowerAddress :: Channel ->  Fpga Registry
getAxiLowerAddress = getAxiGeneric' 0x50

-- | starting writing address ch x - CH x AXI lower address
setAxiUpperAddress :: Channel ->  Registry -> Fpga ()
setAxiUpperAddress = setAxiGeneric' 0x54

-- | read - starting writing address ch x - CH x AXI lower address
getAxiUpperAddress :: Channel ->  Fpga Registry
getAxiUpperAddress = getAxiGeneric' 0x54

-- | read - Number of decimated data after trigger written into memory
getAxiDelayAfterTrigger :: Channel -> Fpga Registry
getAxiDelayAfterTrigger = getAxiGeneric' 0x58

-- | set umber of decimated data after trigger written into memory
setAxiDelayAfterTrigger :: Channel -> Registry -> Fpga ()
setAxiDelayAfterTrigger = setAxiGeneric' 0x58

-- | Enable AXI master
enableAxiMaster :: Channel -> Bool -> Fpga ()
enableAxiMaster ch True = setAxiGeneric' 0x5c ch 1
enableAxiMaster ch False = setAxiGeneric' 0x5c ch 0

-- | Write pointer for ch x at time when trigger arrived
getAxiWritePtrTrigger :: Channel -> Fpga Registry
getAxiWritePtrTrigger = getAxiGeneric' 0x60

-- | current write pointer for ch x
getAxiWritePtrCurrent :: Channel -> Fpga Registry
getAxiWritePtrCurrent = getAxiGeneric' 0x64


-- | reads oscilloscope buffer for channel x from Fpga passing offset and length. 
-- buffer should fit within 16k sampling range.
-- Returns  less than requested data if trying to read over the bounds.
getOscBuffer :: Channel -> Offset -> Int -> Fpga [Registry]
getOscBuffer chan off len = peekFpgaArray osciloscpeFpgaPage (off' + (chOff chan)) len'
                          where
                            off' = max 0 off
                            len' = min (0x10000 - off) len
                            chOff A = 0x10000 
                            chOff B = 0x20000

--------------------------------------------------------------------

-- ASG
-- | Set registry with value passed as tuple of bit offests
-- | setBits (fromBit,toBit) value rin = ..
setBits :: (Int,Int) -> Registry -> Registry -> Registry
setBits (fromBit,toBit) value rin = valueShift .|. hole
    where 
        ones = complement 0 :: Registry
        maskShift = xor (shiftL ones fromBit) (shiftL ones (toBit+1))  
        hole = complement maskShift .&. rin
        valueShift = ( shiftL value fromBit ) .&. maskShift

-- | read bits  range from registy
getBits :: (Int,Int) -> Registry -> Registry
getBits (fromBit,toBit) value = shiftR andV fromBit
    where
        ones = complement 0 :: Registry
        maskShift = xor (shiftL ones fromBit) (shiftL ones (toBit+1))  
        andV = maskShift .&. value


type FpgaSet = Registry -> Fpga ()
type FpgaGet = Fpga Registry 

asgFpgaPage = 2


fpgaWriteAsg :: Offset -> Registry -> Fpga ()
fpgaWriteAsg = fpgaWrite asgFpgaPage

fpgaReadAsg :: Offset -> Fpga Registry
fpgaReadAsg = fpgaRead asgFpgaPage

fpgaFmapAsg :: Offset -> (Registry -> Registry) -> Fpga ()
fpgaFmapAsg = fpgaFmap asgFpgaPage

fpgaWriteAsgChannel :: Offset -> Channel -> Registry -> Fpga ()
fpgaWriteAsgChannel offset A = fpgaWriteAsg   offset
fpgaWriteAsgChannel offset B = fpgaWriteAsg ( offset + 0x20)

fpgaReadAsgChannel :: Offset -> Channel -> Fpga Registry
fpgaReadAsgChannel offset A = fpgaReadAsg   offset
fpgaReadAsgChannel offset B = fpgaReadAsg ( offset + 0x20)

fpgaFmapAsgChannel :: Offset -> (Registry -> Registry) -> Channel -> Fpga ()
fpgaFmapAsgChannel offset f A = fpgaFmapAsg   offset f
fpgaFmapAsgChannel offset f B = fpgaFmapAsg ( offset + 0x20) f


-- | get ASGoption registry
getAsgOption :: Fpga Registry
getAsgOption = fpgaReadAsg 0x0

-- | set ASG option registry
setAsgOption :: Registry -> Fpga ()
setAsgOption = fpgaWriteAsg 0x0


-- | ch B external gated repetitions, 
-- registry can be either 0x0 or 0x1
setAsgOptionBExtGatRep :: Registry -> Fpga ()
setAsgOptionBExtGatRep reg = fpgaFmapAsg 0 ( setBits (24,24) reg)

-- | get ch B external gated repetitions, 
-- registry can be either 0x0 or 0x1
getAsgOptionBExtGatRep :: Fpga Registry
getAsgOptionBExtGatRep =  getBits (24,24) <$> getAsgOption

-- | TODO others

-- | todo other registries

-- | Ch x amplitude scale (14 bist) - out = (data*scale)/0x2000 + offset
setAsgAmplitudeScale :: Channel -> Registry -> Fpga ()
setAsgAmplitudeScale ch reg = fpgaFmapAsgChannel 0x4 ( setBits (16,29) reg ) ch

-- | Ch x amplitude offset (14 bits) - out  = (data*scale)/0x2000 + offset 
setAsgAmplitudeOffset :: Channel -> Registry -> Fpga ()
setAsgAmplitudeOffset ch reg = fpgaFmapAsgChannel 0x4 ( setBits (0,13) reg ) ch

-- | Ch x counter wrap - Value where counter wraps around. Depends on SM wrap setting. 
-- If it is 1 new value is  get by wrap, if value is 0 counter goes to offset value.
-- 16 bits for decimals.
setAsgCounterWrap :: Channel -> Registry -> Fpga ()
setAsgCounterWrap = fpgaWriteAsgChannel  0x8


-- | Ch x Counter start offset. Start offset when trigger arrives. 16 bits for decimals.
setAsgCounterStartOffset :: Channel -> Registry -> Fpga ()
setAsgCounterStartOffset = fpgaWriteAsgChannel 0xc

-- | Ch x counter step. 16 bits for decimals.
setAsgCounterStep :: Channel -> Registry -> Fpga ()
setAsgCounterStep = fpgaWriteAsgChannel 0x10

-- | get ch x buffer current read pointer
getAsgCounterReadPtr :: Channel -> Fpga Registry
getAsgCounterReadPtr = fpgaReadAsgChannel 0x14

-- | set ch x buffer current read pointer
setAsgCounterReadPtr :: Channel -> Registry -> Fpga ()
setAsgCounterReadPtr = fpgaWriteAsgChannel 0x14

-- | get ch x number of read cycles in one burst
getAsgNumReadCycles :: Channel -> Fpga Registry
getAsgNumReadCycles = fpgaReadAsgChannel 0x18

-- | set ch x number of read cycles in one burst
setAsgNumReadCycles :: Channel -> Registry -> Fpga ()
setAsgNumReadCycles = fpgaWriteAsgChannel 0x18

-- | get ch x number of read cycles in one burst
getAsgNumRepetitions :: Channel -> Fpga Registry
getAsgNumRepetitions = fpgaReadAsgChannel  0x1a

-- | set ch x number of read cycles in one burst
setAsgNumRepetitions :: Channel -> Registry -> Fpga ()
setAsgNumRepetitions = fpgaWriteAsgChannel 0x1a

-- | get ch x delay between burst repetitions, granularity=1us
getAsgBurstDelay :: Channel -> Fpga Registry
getAsgBurstDelay = fpgaReadAsgChannel 0x20

-- | set ch x delay between burst repetitions, granularity=1us
setAsgBurstDelay :: Channel -> Registry -> Fpga ()
setAsgBurstDelay = fpgaWriteAsgChannel 0x20

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

        
