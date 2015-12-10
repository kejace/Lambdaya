{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | 
-- <http://redpitaya.com/ Red Pitaya> native library for accessing Fpga

module Fpga2 (
    Registry,
    Channel(..),
    FpgaSetGet(..),
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
    {-
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
    -}
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

-- | type representing fpga memory offset from page
type Offset = Int
-- | type representing fpga memory page
type Page = Int
-- | type representing fpga registry
type Registry = Word32
-- | Fpga monad is hidden behind this type of kind (* -> *)
type FpgaMmapM a = StateT FpgaPtr IO a


class (MonadIO t) => FpgaSetGet t where
    fpgaGet :: (Storable a) => Offset -> t a
    fpgaSet :: (Storable a) => Offset -> a -> t ()
    fpgaGetArray :: (Storable a) => Offset -> Int -> t [a]
    fpgaSetArray :: (Storable a) => Offset -> [a] -> t ()
    -- default implemetations of each others so 
    -- user can provide only one set and one get if requred
    fpgaGet off = fmap head $ fpgaGetArray off 1
    fpgaSet off v = fpgaSetArray off [v]
    fpgaGetArray off len = sequence $ map fpgaGet [off, off+4 .. (off + (4*len))]
    fpgaSetArray off d = sequence_ $ zipWith fpgaSet [off, off+4 .. ] d

-- | Environment where one can read and write Fpga registries
newtype FpgaArm a = FpgaArm ( FpgaMmapM a )
  deriving (Monad,Applicative,Functor,MonadIO,MonadState FpgaPtr)

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

-- | Redpitaya Channel A or B.
data Channel = A | B

getStateType :: FpgaArm a -> FpgaMmapM a
getStateType (FpgaArm s) = s 

-- Constructor
fpgaState a = FpgaArm ( StateT a )

runFpga :: FpgaArm a -> FpgaPtr -> IO (a,FpgaPtr) 
runFpga (FpgaArm s) = runStateT s

getPtr :: FpgaArm FpgaPtr
getPtr = get

-- | This function handles initialising Fpga memory mapping and
-- evaluates 'Fpga' action.
withOpenFpga :: FpgaArm a -> IO a
withOpenFpga act = do
    fd <- openFd  "/dev/mem" ReadWrite Nothing defaultFileFlags
    setFdOption fd SynchronousWrites True
    p <- mmap nullPtr fpgaMapSize (c'PROT_READ + c'PROT_WRITE ) c'MAP_SHARED (fromIntegral fd) addrAms
    (r,s) <- runFpga act p
    munmap p fpgaMapSize
    return r


-- | get raw pointer on fpga registry calculated from page, offset 
-- and internal state that holds memory mapped pointer
-- getOffsetPtr :: Page -> Offset -> FpgaArm (Ptr Registry)
getOffsetPtr page offset = 
    -- offset on getPtr 
    (\memmap -> plusPtr memmap (page * fpgaPageSize + offset)) <$> getPtr


getTotalOffset :: Page -> Offset -> Offset
getTotalOffset page offset = page * fpgaPageSize + offset


-- | direct read from fpga registry
--fpgaRead :: Page -> Offset -> Fpga FpgaMmapM Registry
fpgaRead :: (FpgaSetGet m) => Page -> Offset -> m Registry
fpgaRead page offset = fpgaGet $ getTotalOffset page offset 

-- | direct write in fpga registry
fpgaWrite :: Page ->  Offset -> Registry -> FpgaArm ()
fpgaWrite page offset reg = do
    p <- getOffsetPtr page offset
    liftIO $ poke p reg

-- | apply transformation on fpga registry value 
fpgaFmap :: Page ->  Offset -> (Registry -> Registry) -> FpgaArm ()
fpgaFmap page offset f = do
    reg <- fpgaRead page offset
    fpgaWrite page offset (f reg)

-- | write array in fpga memory
pokeFpgaArray :: Page -> Offset -> [Registry] -> FpgaArm ()
pokeFpgaArray page offset xs =  do
    p <- getOffsetPtr page offset
    liftIO $ pokeArray p xs

-- | read array from fpga memory, passing page, offset and length
peekFpgaArray :: Page -> Offset -> Int -> FpgaArm [Registry]
peekFpgaArray page offset len = do
    p <- getOffsetPtr page offset
    liftIO $ peekArray len p



---------------------------------------------------------
-- * Housekeeping memory map

-- | get ID ,  0 prototype , 1 release
fpgaId :: (FpgaSetGet a) => a Registry
fpgaId = fpgaRead 0 0

-- | get DNA
dna :: (FpgaSetGet a) => a Registry
dna = do
    dna1 <- fromIntegral <$> fpgaRead 0 4
    dna2 <- fromIntegral <$> fpgaRead 0 8
    return $ dna1 + (2^32)*dna2

-- | set expansion connector direction P registry
--
-- 1 out , 0 in  
setExpDirP = fpgaWrite 0 0x10

-- | get expansion connector direction P registry
--
-- 1 out , 0 in 
getExpDirP = fpgaRead 0 0x10

-- | set expansion connector direction N registry
--
-- 1 out , 0 in 
setExpDirN = fpgaWrite 0 0x14

-- | get expansion connector direction N registry
--
-- 1 out , 0 in 
getExpDirN = fpgaRead 0 0x14

-- | expansion connector  P output registry value
setExpOutP = fpgaWrite 0 0x18

-- | expansion connector  P output registry value
getExpOutP = fpgaRead 0 0x18

-- | expansion connector  N output registry value
setExpOutN = fpgaWrite 0 0x1C

-- | expansion connector  N output registry value
getExpOutN = fpgaRead 0 0x1C

-- | expansion connector  P input registry value
getExpInP :: (FpgaSetGet a) => a Registry
getExpInP = fpgaRead 0 0x20

-- | expansion connector  N input registry value
getExpInN :: (FpgaSetGet a) => a Registry
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
-- read using getExpOutN , fmap over setBitValue and bind in setExpOutX
setExpOut N v p = setBitValue v p <$> getExpOutN  >>= setExpOutN
setExpOut P v p = setBitValue v p <$> getExpOutP  >>= setExpOutP


toGpioValue :: Bool -> GpioValue
toGpioValue True = Hi
toGpioValue False = Low

-- | Sets output value of single pin
getExpOut N p = (\x -> toGpioValue ( testBit x p )) <$> getExpOutN
getExpOut P p = (\x -> toGpioValue ( testBit x p )) <$> getExpOutP

-- | write in led registry
setLed = fpgaWrite 0 0x30

-- | read in led registry
getLed :: (FpgaSetGet f) => f Registry
getLed = fpgaRead 0 0x30 


{-
---------------------------------------
-- * Oscilloscope

osciloscpeFpgaPage = 1

fpgaWriteOsc = fpgaWrite osciloscpeFpgaPage

fpgaReadOsc :: (Functor a,FpgaSetGet a) => Offset -> a Registry
fpgaReadOsc = fpgaRead osciloscpeFpgaPage


-- | reset write state machine for oscilloscope
resetWriteSM = fpgaWriteOsc 0 2

-- | start writing data into memory (ARM trigger).
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
setOscTrigger Immediately     = setOscTriggerHelper 1
setOscTrigger ChAPositiveEdge = setOscTriggerHelper 2
setOscTrigger ChANegativeEdge = setOscTriggerHelper 3
setOscTrigger ChBPositiveEdge = setOscTriggerHelper 4
setOscTrigger ChBNegativeEdge = setOscTriggerHelper 5
setOscTrigger ExtPositiveEdge = setOscTriggerHelper 6
setOscTrigger ExtNegaitveEdge = setOscTriggerHelper 7
setOscTrigger AWGPositiveEdge = setOscTriggerHelper 8
setOscTrigger AWGNegativeEdge = setOscTriggerHelper 9


setOscTriggerHelper = fpgaWriteOsc 0x4


-- | when trigger delay is value becomes 'True'
triggerDelayEnded :: (FpgaSetGet a) => a Bool
triggerDelayEnded = (==0) <$> fpgaReadOsc 0x4

-- | Ch x threshold, makes trigger when ADC value cross this value
setTreshold A = fpgaWriteOsc 0x8
setTreshold B = fpgaWriteOsc 0xc

-- | gets ch x threshold
getTreshold A = fpgaReadOsc 0x8
getTreshold B = fpgaReadOsc 0xc

-- | Number of decimated data after trigger written into memory
setDelayAfterTrigger = fpgaWriteOsc 0x10

-- | gets delay after trigger value
getDelayAfterTrigger :: (FpgaSetGet a) =>  a Registry
getDelayAfterTrigger = fpgaReadOsc 0x10

-- | sets oscilloscope decimation registry, allows only
-- 1,8, 64,1024,8192,65536. If other value is written data will NOT be correct.
setOscDecimationRaw =  fpgaWriteOsc 0x14

-- | oscilloscope decimation registry value
getOscDecimationRaw :: (FpgaSetGet a) =>  a Registry
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
setOscDecimation OscDec1 = setOscDecimationRaw 1
setOscDecimation OscDec8 = setOscDecimationRaw 8
setOscDecimation OscDec64 = setOscDecimationRaw 64
setOscDecimation OscDec1024 = setOscDecimationRaw 1024
setOscDecimation OscDec8192 = setOscDecimationRaw 8192
setOscDecimation OscDec65536 = setOscDecimationRaw 65536

-- | write pointer - current
getOscWpCurrent = fpgaReadOsc 0x18

-- | write pointer - trigger
getOscWpTrigger = fpgaReadOsc 0x1C

-- | ch x hysteresis
getOscHysteresis A = fpgaReadOsc 0x20
getOscHysteresis B = fpgaReadOsc 0x24

-- | set ch x hysteresis
setOscHysteresis A = fpgaWriteOsc 0x20
setOscHysteresis B = fpgaWriteOsc 0x24

-- | Enable signal average at decimation True enables, False disables
enableOscDecimationAvarage True = fpgaWriteOsc 0x28 1
enableOscDecimationAvarage False = fpgaWriteOsc 0x28 0

-- | set ch A equalization filter, takes array with coefficients [AA,BB,KK,PP]
setEqualFilter A = pokeFpgaArray osciloscpeFpgaPage 0x30 . take 4
setEqualFilter B = pokeFpgaArray osciloscpeFpgaPage 0x40 . take 4

-- | get ch x equalization filter, return array with coefficients [AA,BB,KK,PP]
getEqualFilter A = peekFpgaArray osciloscpeFpgaPage 0x30 4
getEqualFilter B = peekFpgaArray osciloscpeFpgaPage 0x40 4


setAxiGeneric' offest A = fpgaWriteOsc offest
setAxiGeneric' offest B = fpgaWriteOsc (offest+0x20)

getAxiGeneric' offest A = fpgaReadOsc offest
getAxiGeneric' offest B = fpgaReadOsc (offest+0x20)


-- | starting writing address ch x - CH x AXI lower address
setAxiLowerAddress = setAxiGeneric' 0x50

-- | read - starting writing address ch x - CH x AXI lower address
getAxiLowerAddress = getAxiGeneric' 0x50

-- | starting writing address ch x - CH x AXI lower address
setAxiUpperAddress = setAxiGeneric' 0x54

-- | read - starting writing address ch x - CH x AXI lower address
getAxiUpperAddress = getAxiGeneric' 0x54

-- | read - Number of decimated data after trigger written into memory
getAxiDelayAfterTrigger = getAxiGeneric' 0x58

-- | set umber of decimated data after trigger written into memory
setAxiDelayAfterTrigger = setAxiGeneric' 0x58

-- | Enable AXI master
enableAxiMaster ch True = setAxiGeneric' 0x5c ch 1
enableAxiMaster ch False = setAxiGeneric' 0x5c ch 0

-- | Write pointer for ch x at time when trigger arrived
getAxiWritePtrTrigger :: (Functor a,FpgaSetGet a) => Channel -> a Registry
getAxiWritePtrTrigger = getAxiGeneric' 0x60

-- | current write pointer for ch x
getAxiWritePtrCurrent :: (Functor a,FpgaSetGet a) => Channel -> a Registry
getAxiWritePtrCurrent = getAxiGeneric' 0x64


-- | reads oscilloscope buffer for channel x from Fpga passing offset and length. 
-- buffer should fit within 16k sampling range.
-- Returns  less than requested data if trying to read over the bounds.
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
setBits (fromBit,toBit) value rin = valueShift .|. hole
    where 
        ones = complement 0 :: Registry
        maskShift = xor (shiftL ones fromBit) (shiftL ones (toBit+1))  
        hole = complement maskShift .&. rin
        valueShift = ( shiftL value fromBit ) .&. maskShift

-- | read bits  range from registy
getBits (fromBit,toBit) value = shiftR andV fromBit
    where
        ones = complement 0 :: Registry
        maskShift = xor (shiftL ones fromBit) (shiftL ones (toBit+1))  
        andV = maskShift .&. value




asgFpgaPage = 2


fpgaWriteAsg = fpgaWrite asgFpgaPage

fpgaReadAsg = fpgaRead asgFpgaPage

fpgaFmapAsg = fpgaFmap asgFpgaPage

fpgaWriteAsgChannel offset A = fpgaWriteAsg   offset
fpgaWriteAsgChannel offset B = fpgaWriteAsg ( offset + 0x20)

fpgaReadAsgChannel offset A = fpgaReadAsg   offset
fpgaReadAsgChannel offset B = fpgaReadAsg ( offset + 0x20)

fpgaFmapAsgChannel offset f A = fpgaFmapAsg   offset f
fpgaFmapAsgChannel offset f B = fpgaFmapAsg ( offset + 0x20) f


-- | get ASGoption registry
getAsgOption = fpgaReadAsg 0x0

-- | set ASG option registry
setAsgOption = fpgaWriteAsg 0x0


-- | ch B external gated repetitions, 
-- registry can be either 0x0 or 0x1
setAsgOptionBExtGatRep reg = fpgaFmapAsg 0 ( setBits (24,24) reg)

-- | get ch B external gated repetitions, 
-- registry can be either 0x0 or 0x1
getAsgOptionBExtGatRep =  getBits (24,24) <$> getAsgOption

-- | TODO others

-- | todo other registries

-- | Ch x amplitude scale (14 bist) - out = (data*scale)/0x2000 + offset
setAsgAmplitudeScale ch reg = fpgaFmapAsgChannel 0x4 ( setBits (16,29) reg ) ch

-- | Ch x amplitude offset (14 bits) - out  = (data*scale)/0x2000 + offset 
setAsgAmplitudeOffset ch reg = fpgaFmapAsgChannel 0x4 ( setBits (0,13) reg ) ch

-- | Ch x counter wrap - Value where counter wraps around. Depends on SM wrap setting. 
-- If it is 1 new value is  get by wrap, if value is 0 counter goes to offset value.
-- 16 bits for decimals.
setAsgCounterWrap = fpgaWriteAsgChannel  0x8


-- | Ch x Counter start offset. Start offset when trigger arrives. 16 bits for decimals.
setAsgCounterStartOffset = fpgaWriteAsgChannel 0xc

-- | Ch x counter step. 16 bits for decimals.
setAsgCounterStep = fpgaWriteAsgChannel 0x10

-- | get ch x buffer current read pointer
getAsgCounterReadPtr = fpgaReadAsgChannel 0x14

-- | set ch x buffer current read pointer
setAsgCounterReadPtr = fpgaWriteAsgChannel 0x14

-- | get ch x number of read cycles in one burst
getAsgNumReadCycles = fpgaReadAsgChannel 0x18

-- | set ch x number of read cycles in one burst
setAsgNumReadCycles = fpgaWriteAsgChannel 0x18

-- | get ch x number of read cycles in one burst
getAsgNumRepetitions = fpgaReadAsgChannel  0x1a

-- | set ch x number of read cycles in one burst
setAsgNumRepetitions = fpgaWriteAsgChannel 0x1a

-- | get ch x delay between burst repetitions, granularity=1us
getAsgBurstDelay = fpgaReadAsgChannel 0x20

-- | set ch x delay between burst repetitions, granularity=1us
setAsgBurstDelay = fpgaWriteAsgChannel 0x20

-}

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





main = print "haha"

        
