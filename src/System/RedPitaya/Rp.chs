
module System.RedPitaya.Rp (
        RpMonad(..),
        
        
        
         
        
        
        
        
        
        
        
        
        -- * General
        rpinit,
        reset,
        release,
        pinsReset,
        getVersion,
        getError,

        -- * Digital IO
        DigitalPin(..), 

        PinState(..),
        setPinState ,
        getPinState,

        PinDirection(..),
        setPinDirection ,
        getPinDirection,

        -- * Analog IO
        AnalogPin(..),
        getPinValue,
        setPinValue,
        getPinValueRaw ,
        setPinValueRaw,
        getPinRange,

        Decimation(..), 
        setDecimation,
        getDecimation,
        getDecimationFactor,
        
        SamplingRate(..), 
        setSamplingRate,
        getSamplingRate,
        getSamplingRateHz,
        setAvaraging,
        getAvaraging,
        
        TriggerSource(..),
        setTriggerSrc,
        getTriggerSrc,
        
        TriggerState(..), 
        getTriggerState,
        getTriggerDelay,
        setTriggerDelayNs,
        getTriggerDelayNs,
        setTriggerLevel,
        getTriggerLevel,
        setTriggerLevelHyst,
        getTriggerLevelHyst,

        -- * Acquision 
        Channel(..), 
        TriggerSourceAcq(..),
        setAcqGain,
        getAcqGain,
        getAcqGainV,
        getAcqWritePointer,
        getAcqWritePointerAtTrg,
        acqStart,
        acqReset,
        getNormalisedDataPosition,
        getAckDataRaw,
        getAckDataV,
        getAckOldestDataRaw,
        getAckLatestDataRaw,
        getAckOldestDataV,
        getAckLatestDataV,
        getAcqBufSize,

        -- * Generator
        Waveform(..), 
        GeneratorMode(..), 
        generatorReset,
        generatorOutEnable,
        generatorOutDisable,
        generatorOutIsEnabled,
        generatorAmp,
        getGeneratorAmp,
        setGeneratorOffset,
        generatorFrequency,
        generatorPhase,
        generatorWaveForm,
        generatorDutyCycle,
        generatorMode,
        generatorBursts,
        generatorBurstsRepetitions,
        generatorBurstsPeriod,
        generatorTriggerSource,

        -- * Health 
        Health(..),
        getHealthValue,

        RpError(..), 
    )
where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Marshal.Array

import Data.Functor
import Control.Monad
import Data.Int
import Control.Monad.Except

#c
#include "rp.h"
#endc


-- | main monad functions of this lib lives in
type RpMonad = ExceptT RpError IO

-- maprX maps over final return value 
mapr0 :: (a->b) ->a->b
mapr0 = ($)

mapr1 :: (a->b) ->(c->a)->(c->b)
mapr1 = (.)

mapr2 :: (a->b) ->(c->d->a)->(c->d->b)
mapr2 = (.) . (.)

mapr3 :: (a->b) ->(c->d->e->a)->(c->d->e->b)
mapr3 = mapr2 . (.)

mapr4 :: (a->b) ->(c->d->e->f->a)->(c->d->e->f->b)
mapr4 = mapr3 . (.)

-- ftX makes n-tuple 2-tuple
ft1 :: (a) -> (a,())
ft1 (a) = (a,())

ft2 :: (a,b) -> (a,b)
ft2  = id

ft3 :: (a,b,c) -> (a,(b,c))
ft3 (a,b,c) = (a,(b,c))

ft4 :: (a,b,c,d) -> (a,(b,c,d))
ft4 (a,b,c,d) = (a,(b,c,d))

ft5 :: (a,b,c,d,e) -> (a,(b,c,d,e))
ft5 (a,b,c,d,e) = (a,(b,c,d,e))

errorMap :: (a -> (RpError,r)) -> IO a -> RpMonad r
errorMap ftfun etup = do
    t <- liftIO etup
    let (e,res) = ftfun t
    if (e /= Ok)
    then
        throwError e
    else
        return res


-- | Type representing digital input output pins.
{#enum rp_dpin_t as DigitalPin {underscoreToCase} with prefix = "RP_" deriving (Eq, Show) #}

-- | Type representing pin's high or low state (on/off).
{#enum rp_pinState_t as PinState {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}

-- | Type representing pin's input or output direction.
{#enum rp_pinDirection_t as PinDirection {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}

-- | Type representing analog input output pins.
{#enum rp_apin_t as AnalogPin {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}

-- | Type representing system health information.
{#enum rp_health_t as Health {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}

-- | Type representing shape of generated waveform
{#enum rp_waveform_t as Waveform {underscoreToCase} with prefix = "RP_WAVEFORM_" deriving (Eq, Show)#}

-- | Type representing mode of generated waveform
{#enum rp_gen_mode_t as GeneratorMode {underscoreToCase} with prefix = "RP_GEN_MODE_" deriving (Eq, Show)#}

--------------------------------------------

-- | Type representing source of trigger that is either ,
-- internal, 
-- external with positve edge, 
-- external with negative edge, 
-- external trigger gated burst
{#enum rp_trig_src_t as TriggerSource {underscoreToCase} with prefix = "RP_GEN_TRIG_" deriving (Eq, Show)#}


-- | Type representing Input Output channels.
{#enum rp_channel_t as Channel {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}

-- | Type representing acquire signal sampling rate.
{#enum rp_acq_sampling_rate_t as SamplingRate {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}


-- | Type representing decimation used at acquiring signal.
{#enum rp_acq_decimation_t as Decimation {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}

-- | Type representing different trigger sources used at acquiring signal.
{#enum rp_acq_trig_src_t as TriggerSourceAcq {underscoreToCase} with prefix = "RP_TRIG_SRC_" deriving (Eq, Show)#}

-- | Type representing different trigger states.
{#enum rp_acq_trig_state_t as TriggerState {underscoreToCase} with prefix = "RP_TRIG_STATE_" deriving (Eq, Show)#}



-- | Type representing errors returend by each function
{#enum define RpError {
    RP_OK as Ok,
    RP_EOED as OpenEeprom,
    RP_EOMD as OpenMemDev,
    RP_ECMD as CloseMemDev,
    RP_EMMD as MapMemDev,
    RP_EUMD as UnampMemDev,
    RP_EOOR as ValueOutOfRange,
    RP_ELID as LedDir,
    RP_EMRO as ModifyingReadOnly,
    RP_EWIP as WritingInputPin,
    RP_EPN as InvalidPinNum,
    RP_UIA as UnitialisedInputArg,
    RP_FCA as FailedToFindCalibParam,
    RP_RCA as FailedToReadCalibParam,
    RP_BTS as BufferTooSmall,
    RP_EIPV as InvalidParameter,
    RP_EUF as UnsupportedFeature,
    RP_ENN as DataNotNormalised,
    RP_EFOB as FailedToOpenBus,
    RP_EFCB as FailedToCloseBus,
    RP_EABA as FailedToAcquireBus,
    RP_EFRB as FailedToReadBus,
    RP_EFWB as FailedToWriteBus}
    deriving (Eq, Show)
#}

peekMap :: Storable a => ( a -> b ) -> ( Ptr a -> IO b )
peekMap f = \x -> ( f <$>  (peek  x ))

peekEnum :: Enum a => Ptr CInt -> IO a
peekEnum =  peekMap (toEnum . fromIntegral )

peekInt :: (Integral a , Storable a) => Ptr a -> IO Integer
peekInt =  peekMap  toInteger

peekBool :: (Integral a , Storable a) => Ptr a -> IO Bool
peekBool =  peekMap  (/= 0)

-- | Initializes the library. It must be called first, before any other library method.
rpinit :: RpMonad ()
rpinit = mapr0 ( errorMap ft1) rp_Init
{#fun rp_Init  {} -> `RpError' #}


-- | Resets all modules. Typically called after `rpinit`
reset :: RpMonad ()
reset = mapr0 ( errorMap ft1) rp_Reset
{#fun rp_Reset {} -> `RpError' #}

-- | Releases the library resources. It must be called last, after library is not used anymore. Typically before
-- application exits.
release :: RpMonad ()
release = mapr0 ( errorMap ft1) rp_Release
{#fun rp_Release {} -> `RpError' #}

-- | Sets digital pins to default values. Pins DIO1_P - DIO7_P, RP_DIO0_N - RP_DIO7_N are set al OUTPUT and to LOW. LEDs are set to LOW/OFF
pinsReset :: RpMonad ()
pinsReset = mapr0 ( errorMap ft1) rp_DpinReset
{#fun rp_DpinReset {} -> `RpError' #}

-- | Retrieves the library librp version number
getVersion :: RpMonad String
getVersion = liftIO rp_GetVersion
{#fun rp_GetVersion  {} -> `String' #}

-- | textual representation of error code
{#fun rp_GetError as getError  {`RpError'} -> `String' #}


-- | Sets digital pins to default values. Pins DIO1_P - DIO7_P, RP_DIO0_N - RP_DIO7_N are set al OUTPUT and to LOW. LEDs are set to LOW/OFF
setPinState :: DigitalPin -> PinState -> RpMonad ()
setPinState = mapr2 ( errorMap ft1) rp_DpinSetState
{#fun rp_DpinSetState { `DigitalPin' , `PinState' } -> `RpError' #}

-- | Get pins enabled values
getPinState :: DigitalPin -> RpMonad PinState
getPinState = mapr1 ( errorMap ft2) rp_DpinGetState
{#fun rp_DpinGetState { `DigitalPin' ,  alloca- `PinState' peekEnum* } -> `RpError' #}




-- | Sets digital input output pin direction. LED pins are already automatically set to the output direction,
-- and they cannot be set to the input direction. DIOx_P and DIOx_N are must set either output or input direction
-- before they can be used. When set to input direction, it is not allowed to write into these pins.
setPinDirection :: DigitalPin -> PinDirection -> RpMonad ()
setPinDirection = mapr2 ( errorMap ft1) rp_DpinSetDirection
{#fun rp_DpinSetDirection { `DigitalPin' , `PinDirection' } -> `RpError' #}

-- gets digital input output pin direction
getPinDirection :: DigitalPin  -> RpMonad PinDirection
getPinDirection = mapr1 ( errorMap ft2) rp_DpinGetDirection
{#fun rp_DpinGetDirection { `DigitalPin' , alloca- `PinDirection' peekEnum* } -> `RpError' #}

-- | Sets analog pins to default values. Output pins are set to 0 V.
analogPinsReset ::  RpMonad ()
analogPinsReset = mapr0 ( errorMap ft1) rp_ApinReset
{#fun rp_ApinReset  {} -> `RpError' #}


-- | Gets value from analog pin in volts.
getPinValue ::  AnalogPin -> RpMonad CFloat
getPinValue = mapr1 ( errorMap ft2) rp_ApinGetValue
{#fun rp_ApinGetValue  { `AnalogPin' ,  alloca- `CFloat' peek* } -> `RpError' #}

-- | Gets raw value from analog pin.
getPinValueRaw ::  AnalogPin -> RpMonad Integer
getPinValueRaw = mapr1 ( errorMap ft2) rp_ApinGetValueRaw
{#fun rp_ApinGetValueRaw  { `AnalogPin' ,  alloca- `Integer' peekInt* } -> `RpError' #}

-- | Sets value in volts on analog output pin
setPinValue ::  AnalogPin -> CFloat -> RpMonad ()
setPinValue = mapr2 ( errorMap ft1) rp_ApinSetValue
{#fun rp_ApinSetValue { `AnalogPin' ,  `CFloat'} -> `RpError' #}

-- | Sets raw value on analog output pin.
setPinValueRaw ::  AnalogPin -> Int -> RpMonad ()
setPinValueRaw = mapr2 ( errorMap ft1) rp_ApinSetValueRaw
{#fun rp_ApinSetValueRaw { `AnalogPin' , `Int'} -> `RpError' #}

-- | Gets range in volts on specific pin
getPinRange ::  AnalogPin ->  RpMonad (CFloat,CFloat)
getPinRange = mapr1 ( errorMap ft3) rp_ApinGetRange
{#fun rp_ApinGetRange  { `AnalogPin' ,  alloca- `CFloat' peek* ,  alloca- `CFloat' peek* } -> `RpError' #}


-- | Sets the decimation used at acquiring signal. There is only a set of pre-defined decimation
setDecimation ::  Decimation ->  RpMonad ()
setDecimation = mapr1 ( errorMap ft1) rp_AcqSetDecimation
{#fun rp_AcqSetDecimation {`Decimation'} -> `RpError' #}

-- | Gets the decimation used at acquiring signal.
getDecimation ::  RpMonad Decimation
getDecimation = mapr0 ( errorMap ft2) rp_AcqGetDecimation
{#fun rp_AcqGetDecimation  {alloca- `Decimation' peekEnum*} -> `RpError' #}

-- | Gets the decimation factor used at acquiring signal in a numerical form.
getDecimationFactor ::  RpMonad Integer
getDecimationFactor = mapr0 ( errorMap ft2) rp_AcqGetDecimationFactor
{#fun rp_AcqGetDecimationFactor {alloca- `Integer' peekInt*} -> `RpError' #}

-- | Sets the sampling rate for acquiring signal.
setSamplingRate ::  SamplingRate -> RpMonad ()
setSamplingRate = mapr1 ( errorMap ft1) rp_AcqSetSamplingRate
{#fun rp_AcqSetSamplingRate  {`SamplingRate'} -> `RpError' #}

-- | Gets the sampling rate for acquiring signal
getSamplingRate ::  RpMonad SamplingRate
getSamplingRate = mapr0 ( errorMap ft2) rp_AcqGetSamplingRate
{#fun rp_AcqGetSamplingRate { alloca- `SamplingRate' peekEnum* } -> `RpError' #}

-- | Gets the sampling rate for acquiring signal in a numerical form in Hz. Although this method returns a float
-- value representing the current value of the sampling rate, there is only a set of pre-defined sampling rate
-- values which can be returned.
getSamplingRateHz ::  RpMonad CFloat
getSamplingRateHz = mapr0 ( errorMap ft2) rp_AcqGetSamplingRateHz
{#fun rp_AcqGetSamplingRateHz  { alloca- `CFloat' peek* } -> `RpError' #}

-- | Enables or disables averaging of data between samples.
-- Data between samples can be averaged by setting the averaging flag in the Data decimation register.
setAvaraging ::  Bool -> RpMonad ()
setAvaraging = mapr1 ( errorMap ft1) rp_AcqSetAveraging
{#fun rp_AcqSetAveraging   {`Bool'} -> `RpError' #}

-- | Returns information if averaging of data between samples is enabled or disabled.
getAvaraging ::  RpMonad Bool
getAvaraging = mapr0 ( errorMap ft2) rp_AcqGetAveraging
{#fun rp_AcqGetAveraging   { alloca- `Bool' peekBool*} -> `RpError' #}

-- | Sets the trigger source used at acquiring signal. When acquiring is started,
-- the FPGA waits for the trigger condition on the specified source and when the condition is met, it
-- starts writing the signal to the buffer.
setTriggerSrc :: TriggerSource -> RpMonad ()
setTriggerSrc = mapr1 ( errorMap ft1) rp_AcqSetTriggerSrc
{#fun rp_AcqSetTriggerSrc {`TriggerSource'} -> `RpError' #}

-- | Gets the trigger source used at acquiring signal.
getTriggerSrc :: RpMonad TriggerSource
getTriggerSrc = mapr0 ( errorMap ft2) rp_AcqGetTriggerSrc
{#fun rp_AcqGetTriggerSrc {alloca- `TriggerSource' peekEnum*} -> `RpError' #}

-- | Returns the trigger state. Either it is waiting for a trigger to happen, or it has already been triggered.
-- By default it is in the triggered state, which is treated the same as disabled.
getTriggerState :: RpMonad TriggerState
getTriggerState = mapr0 ( errorMap ft2) rp_AcqGetTriggerState
{#fun rp_AcqGetTriggerState {alloca- `TriggerState' peekEnum*} -> `RpError' #}

-- | Sets the number of decimated data after trigger written into memory
setTriggerDelay :: Integer -> RpMonad ()
setTriggerDelay = mapr1 ( errorMap ft1) rp_AcqSetTriggerDelay
{#fun rp_AcqSetTriggerDelay  { fromIntegral `Integer' } -> `RpError' #}

-- | Returns current number of decimated data after trigger written into memory.
getTriggerDelay ::  RpMonad Integer
getTriggerDelay = mapr0 ( errorMap ft2) rp_AcqGetTriggerDelay
{#fun rp_AcqGetTriggerDelay { alloca- `Integer' peekInt* } -> `RpError' #}

-- | Sets the amount of decimated data in nanoseconds after trigger written into memory.
setTriggerDelayNs ::  Integer -> RpMonad ()
setTriggerDelayNs = mapr1 ( errorMap ft1) rp_AcqSetTriggerDelayNs
{#fun rp_AcqSetTriggerDelayNs { fromIntegral `Integer'  } -> `RpError' #}

-- | Returns the current amount of decimated data in nanoseconds after trigger written into memory.
getTriggerDelayNs ::  RpMonad Integer
getTriggerDelayNs = mapr0 ( errorMap ft2) rp_AcqGetTriggerDelayNs
{#fun rp_AcqGetTriggerDelayNs { alloca- `Integer' peekInt* } -> `RpError' #}

-- | Sets the trigger threshold value in volts. Makes the trigger when ADC value crosses this value.
setTriggerLevel ::  CFloat -> RpMonad ()
setTriggerLevel = mapr1 ( errorMap ft1) rp_AcqSetTriggerLevel
{#fun rp_AcqSetTriggerLevel { `CFloat' } -> `RpError' #}

-- | Gets currently set trigger threshold value in volts
getTriggerLevel ::  RpMonad CFloat
getTriggerLevel = mapr0 ( errorMap ft2) rp_AcqGetTriggerLevel
{#fun rp_AcqGetTriggerLevel { alloca- `CFloat' peek* } -> `RpError' #}

-- | Sets the trigger threshold hysteresis value in volts.
-- Value must be outside to enable the trigger again.
setTriggerLevelHyst ::  CFloat -> RpMonad ()
setTriggerLevelHyst = mapr1 ( errorMap ft1) rp_AcqSetTriggerHyst
{#fun rp_AcqSetTriggerHyst { `CFloat' } -> `RpError' #}

-- | Gets currently set trigger threshold hysteresis value in volts
getTriggerLevelHyst :: RpMonad CFloat
getTriggerLevelHyst = mapr0 ( errorMap ft2) rp_AcqGetTriggerHyst
{#fun rp_AcqGetTriggerHyst { alloca- `CFloat' peek* } -> `RpError' #}

-- | Sets the acquire gain state. The gain should be set to the same value as it is set on the Red Pitaya
setAcqGain :: Channel -> PinState -> RpMonad ()
setAcqGain = mapr2 ( errorMap ft1) rp_AcqSetGain
{#fun rp_AcqSetGain { `Channel' , `PinState' } -> `RpError' #}

-- | Returns the currently set acquire gain state in the library. It may not be set to the same value as
-- it is set on the Red Pitaya hardware by the LV/HV gain jumpers. LV = 1V; HV = 20V.
getAcqGain :: Channel -> RpMonad PinState
getAcqGain = mapr1 ( errorMap ft2) rp_AcqGetGain
{#fun rp_AcqGetGain { `Channel' , alloca- `PinState' peekEnum* } -> `RpError' #}

-- | Returns the currently set acquire gain in the library. It may not be set to the same value as
-- it is set on the Red Pitaya hardware by the LV/HV gain jumpers. Returns value in Volts.
getAcqGainV :: Channel -> RpMonad CFloat
getAcqGainV = mapr1 ( errorMap ft2) rp_AcqGetGainV
{#fun rp_AcqGetGainV { `Channel' , alloca- `CFloat' peek* } -> `RpError' #}

-- | Returns current position of ADC write pointer.
getAcqWritePointer :: RpMonad Integer
getAcqWritePointer = mapr0 ( errorMap ft2) rp_AcqGetWritePointer
{#fun rp_AcqGetWritePointer { alloca- `Integer' peekInt* } -> `RpError' #}

-- | Returns position of ADC write pointer at time when trigger arrived.
getAcqWritePointerAtTrg :: RpMonad Integer
getAcqWritePointerAtTrg = mapr0 ( errorMap ft2) rp_AcqGetWritePointerAtTrig
{#fun rp_AcqGetWritePointerAtTrig { alloca- `Integer' peekInt* } -> `RpError' #}

-- | Starts the acquire. Signals coming from the input channels are acquired and written into memory.
acqStart :: RpMonad ()
acqStart = mapr0 ( errorMap ft1) rp_AcqStart
{#fun rp_AcqStart {} -> `RpError' #}

-- | Resets the acquire writing state machine.
acqReset :: RpMonad ()
acqReset = mapr0 ( errorMap ft1) rp_AcqReset
{#fun rp_AcqReset {} -> `RpError' #}


-- | Normalizes the ADC buffer position. Returns the modulo operation of ADC buffer size...
getNormalisedDataPosition :: Integer -> RpMonad Integer
getNormalisedDataPosition = liftIO . rp_AcqGetNormalizedDataPos
{#fun rp_AcqGetNormalizedDataPos { fromIntegral `Integer' } ->  `Integer' fromIntegral #}

getPosSizeBufferHelper :: (Storable a) => (CInt -> (CUInt -> ((Ptr CUInt) -> ((Ptr a) -> (IO CInt))))) -> Channel -> Int -> Int -> IO ( RpError , [a])
getPosSizeBufferHelper ffiFunc chan pos size = allocaArray  (fromIntegral size)  $
    \bufptr -> alloca $
        \sizeptr ->  do
            let echan  = fromIntegral $ fromEnum chan
            let ipos = fromIntegral pos
            err <- ffiFunc echan ipos sizeptr bufptr
            if (err /= 0) then
                do
                    len <- peek  sizeptr
                    arr <- peekArray (fromIntegral len) bufptr
                    return ( Ok , arr)
            else return ( toEnum $ fromIntegral err , [])

getSizeBufferHelper :: (Storable a) => (CInt -> Ptr CUInt -> Ptr a -> IO CInt) -> Int -> Channel -> IO ( RpError , [a])
getSizeBufferHelper ffiFunc allocSize chan = allocaArray  allocSize $
    \bufptr -> alloca $
        \sizeptr ->  do
            let echan  = fromIntegral $ fromEnum chan
            _ <- poke sizeptr (fromIntegral allocSize)
            err <- ffiFunc echan sizeptr bufptr
            if (err /= 0) then
                do
                    len <- peek  sizeptr
                    arr <- peekArray (fromIntegral len) bufptr
                    return ( Ok , arr)
            else return ( toEnum $ fromIntegral err , [])


-- | get raw ADC data from channel passing position and size
getAckDataRaw :: Channel -> Int -> Int -> RpMonad [CShort]
getAckDataRaw = mapr3 ( errorMap ft2) $ getPosSizeBufferHelper {#call rp_AcqGetDataRaw#}

-- | get  ADC data Voltage from channel passing position and size
getAckDataV :: Channel -> Int -> Int -> RpMonad [CFloat]
getAckDataV = mapr3 ( errorMap ft2) $ getPosSizeBufferHelper {#call rp_AcqGetDataV#}

-- | Returns the ADC buffer in raw units from the oldest sample to the newest one.
-- First parameter is maximal length of returned data (how large buffer to allocate)
getAckOldestDataRaw :: Int -> Channel ->  RpMonad [CShort]
getAckOldestDataRaw = mapr2 ( errorMap ft2) $ getSizeBufferHelper {#call rp_AcqGetOldestDataRaw#}

-- | Returns the ADC buffer in raw units from the latest
-- First parameter is maximal length of returned data (how large buffer to allocate)
getAckLatestDataRaw :: Int -> Channel ->  RpMonad [CShort]
getAckLatestDataRaw = mapr2 ( errorMap ft2) $ getSizeBufferHelper {#call rp_AcqGetLatestDataRaw#}

-- | Returns the ADC buffer in volts from the oldest sample to the newest one.
-- First parameter is maximal length of returned list (how large buffer to allocate)
getAckOldestDataV :: Int -> Channel ->  RpMonad [CFloat]
getAckOldestDataV = mapr2 ( errorMap ft2) $ getSizeBufferHelper {#call rp_AcqGetOldestDataV#}

-- | Returns the ADC buffer in volts from the latest
-- First parameter is maximal length of returned list (how large buffer to allocate)
getAckLatestDataV :: Int -> Channel ->  RpMonad [CFloat]
getAckLatestDataV = mapr2 ( errorMap ft2) $  getSizeBufferHelper {#call rp_AcqGetLatestDataV#}

-- | Returns size of acquistion buffer
getAcqBufSize :: RpMonad Integer
getAcqBufSize = mapr0 ( errorMap ft2) rp_AcqGetBufSize
{#fun rp_AcqGetBufSize { alloca- `Integer' peekInt* } -> `RpError' #}

-- | Gets data about system health
getHealthValue :: Health -> RpMonad CFloat
getHealthValue = mapr1 ( errorMap ft2) rp_HealthGetValue
{#fun rp_HealthGetValue { `Health' ,  alloca- `CFloat' peek* } -> `RpError' #}

-- | Sets generate to default values.
generatorReset ::  RpMonad ()
generatorReset = mapr0 ( errorMap ft1) rp_GenReset
{#fun rp_GenReset {} -> `RpError' #}

-- | Enables output
generatorOutEnable ::  Channel -> RpMonad ()
generatorOutEnable = mapr1 ( errorMap ft1) rp_GenOutEnable
{#fun rp_GenOutEnable {`Channel'} -> `RpError' #}

-- | Disables output
generatorOutDisable ::  Channel -> RpMonad ()
generatorOutDisable = mapr1 ( errorMap ft1) rp_GenOutDisable
{#fun rp_GenOutDisable {`Channel'} -> `RpError' #}

-- | Gets value `True` if channel is enabled otherwise return `False`
generatorOutIsEnabled ::   Channel -> RpMonad Bool
generatorOutIsEnabled = mapr1 ( errorMap ft2) rp_GenOutIsEnabled
{#fun rp_GenOutIsEnabled {`Channel', alloca- `Bool' peekBool*} -> `RpError' #}

-- | Sets channel signal peak to peak amplitude.
generatorAmp ::   Channel -> CFloat -> RpMonad ()
generatorAmp = mapr2 ( errorMap ft1) rp_GenAmp
{#fun rp_GenAmp {`Channel' , `CFloat' } -> `RpError' #}

-- | gets channel signal peak to peak amplitude
getGeneratorAmp ::   Channel -> RpMonad CFloat
getGeneratorAmp = mapr1 ( errorMap ft2) rp_GenGetAmp
{#fun rp_GenGetAmp {`Channel' , alloca- `CFloat' peek* } -> `RpError' #}


-- | Sets DC offset of the signal. signal = signal + DC_offset.
setGeneratorOffset ::   Channel -> CFloat -> RpMonad ()
setGeneratorOffset = mapr2 ( errorMap ft1) rp_GenOffset
{#fun rp_GenOffset {`Channel' , `CFloat' } -> `RpError' #}

-- | Sets channel signal frequency.
generatorFrequency ::   Channel -> CFloat -> RpMonad ()
generatorFrequency = mapr2 ( errorMap ft1) rp_GenFreq
{#fun rp_GenFreq {`Channel' , `CFloat' } -> `RpError' #}

-- | Sets channel signal phase. This shifts the signal in time.
generatorPhase ::   Channel -> CFloat -> RpMonad ()
generatorPhase = mapr2 ( errorMap ft1) rp_GenPhase
{#fun rp_GenPhase {`Channel' , `CFloat' } -> `RpError' #}


-- | Sets channel signal phase. This shifts the signal in time.
generatorWaveForm ::   Channel -> Waveform -> RpMonad ()
generatorWaveForm = mapr2 ( errorMap ft1) rp_GenWaveform
{#fun rp_GenWaveform  {`Channel' , `Waveform' } -> `RpError' #}


setBufferHelper :: (Storable a,Integral i) => (Ptr a -> i -> IO r) -> [a] -> IO r
setBufferHelper fun xs = withArrayLen xs (\i p -> fun p (fromIntegral i))

-- | Sets user defined waveform
generatorArbWaveForm ::   Channel -> [CFloat] -> RpMonad ()
generatorArbWaveForm = mapr2 ( errorMap ft1) gawf
    where 
            gawf :: Channel -> [CFloat] -> IO RpError
            gawf c xs = (toEnum . fromIntegral) <$>
                setBufferHelper ({#call rp_GenArbWaveform #}
                ((fromIntegral . fromEnum) c)) xs


-- | Sets duty cycle of PWM signal.
generatorDutyCycle ::   Channel -> CFloat -> RpMonad ()
generatorDutyCycle = mapr2 ( errorMap ft1) rp_GenDutyCycle
{#fun rp_GenDutyCycle {`Channel' , `CFloat' } -> `RpError' #}

-- | Sets generation mode.
generatorMode ::   Channel -> GeneratorMode -> RpMonad ()
generatorMode = mapr2 ( errorMap ft1) rp_GenMode
{#fun rp_GenMode {`Channel' , `GeneratorMode' } -> `RpError' #}

-- | Sets number of generated waveforms in a burst. If -1 a continuous signal will be generated.
generatorBursts ::   Channel -> Integer -> RpMonad ()
generatorBursts = mapr2 ( errorMap ft1) rp_GenBurstCount
{#fun rp_GenBurstCount {`Channel' , fromInteger `Integer' } -> `RpError' #}

-- | Sets number of burst repetitions. This determines how many bursts will be generated.
-- If -1 a continuous signal will be generated.
generatorBurstsRepetitions ::   Channel -> Integer -> RpMonad ()
generatorBurstsRepetitions = mapr2 ( errorMap ft1) rp_GenBurstRepetitions
{#fun rp_GenBurstRepetitions {`Channel' , fromInteger `Integer' } -> `RpError' #}


-- | Sets the time/period of one burst in micro seconds. Period must be equal or greater then the time of one burst.
-- If it is greater than the difference will be the delay between two consequential bursts.
generatorBurstsPeriod ::   Channel -> Integer -> RpMonad ()
generatorBurstsPeriod = mapr2 ( errorMap ft1) rp_GenBurstPeriod
{#fun rp_GenBurstPeriod {`Channel' , fromInteger `Integer' } -> `RpError' #}

-- | Sets trigger source.
generatorTriggerSource ::   Channel -> TriggerSource -> RpMonad ()
generatorTriggerSource = mapr2 ( errorMap ft1) rp_GenTriggerSource
{#fun rp_GenTriggerSource {`Channel' , `TriggerSource' } -> `RpError' #}
