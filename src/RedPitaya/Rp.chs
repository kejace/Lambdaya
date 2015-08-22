
module RedPitaya.Rp (DigitalPin(..), PinState(..), PinDirection(..), AnalogPin(..),
        Health(..), Waveform(..), GeneratorMode(..), TriggerSource(..),
        Channel(..), SamplingRate(..), Decimation(..), TriggerSourceAcq(..),
        TriggerState(..), RpError(..),
        rpinit, 
        reset, 
        release, 
        pinsReset,  
        getVersion, 
        getError,
        setPinState , 
        getPinState, 
        setPinDirection , 
        getPinDirection,
        getPinValue, 
        setPinValue, 
        getPinValueRaw , 
        setPinValueRaw,
        getPinRange, 
        setDecimation, 
        getDecimation, 
        getDecimationFactor,
        setSamplingRate,
        getSamplingRate,
        getSamplingRateHz,
        enableAvaraging,
        getAvaraging,
        setTriggerSrc,
        getTriggerSrc,
        getTriggerState,
        getTriggerDelay,
        setTriggerDelayNs,
        getTriggerDelayNs,
        setTriggerLevel,
        getTriggerLevel,
        setTriggerLevelHyst,
        getTriggerLevelHyst,
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
        getHealthValue,
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
        generatorTriggerSource
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

#c
#include "rp.h"
#endc

-- | helpers for wrapping
mapr0 :: (a->b) ->a->b
mapr0 = ($)  

-- | helpers for wrapping
mapr1 :: (a->b) ->(c->a)->(c->b)
mapr1 = (.)  

mapr2 :: (a->b) ->(c->d->a)->(c->d->b)
mapr2 = (.) . (.)
 
mapr3 :: (a->b) ->(c->d->e->a)->(c->d->e->b)
mapr3 = mapr2 . (.)

mapr4 :: (a->b) ->(c->d->e->f->a)->(c->d->e->f->b)
mapr4 = mapr3 . (.)

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

-- |  Type representing shape of generated waveform
{#enum rp_waveform_t as Waveform {underscoreToCase} with prefix = "RP_WAVEFORM_" deriving (Eq, Show)#}


{#enum rp_gen_mode_t as GeneratorMode {underscoreToCase} with prefix = "RP_GEN_MODE_" deriving (Eq, Show)#}

{#enum rp_trig_src_t as TriggerSource {underscoreToCase} with prefix = "RP_GEN_TRIG_" deriving (Eq, Show)#}

-- | Type representing Input/Output channels.
{#enum rp_channel_t as Channel {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}

-- |  Type representing acquire signal sampling rate.
{#enum rp_acq_sampling_rate_t as SamplingRate {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}

-- |  Type representing decimation used at acquiring signal.
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
{#fun rp_Init as rpinit  {} -> `RpError' #}

-- | Resets all modules. Typically called after `rpinit`
{#fun rp_Reset as reset  {} -> `RpError' #}

-- | Releases the library resources. It must be called last, after library is not used anymore. Typically before
-- application exits.
{#fun rp_Release as release  {} -> `RpError' #}

-- |  Sets digital pins to default values. Pins DIO1_P - DIO7_P, RP_DIO0_N - RP_DIO7_N are set al OUTPUT and to LOW. LEDs are set to LOW/OFF
{#fun rp_DpinReset as pinsReset  {} -> `RpError' #}

-- | Retrieves the library librp version number
{#fun rp_GetVersion as getVersion  {} -> `String' #}

-- | textual representation of error code
{#fun rp_GetError as getError  {`RpError'} -> `String' #}

----------------------------------------------------------------

-- |  Sets digital pins to default values. Pins DIO1_P - DIO7_P, RP_DIO0_N - RP_DIO7_N are set al OUTPUT and to LOW. LEDs are set to LOW/OFF
{#fun rp_DpinSetState as setPinState { `DigitalPin' , `PinState' } -> `RpError' #}

{#fun rp_DpinGetState as getPinState { `DigitalPin' ,  alloca- `PinState' peekEnum* } -> `RpError' #}

-- | Sets digital input output pin direction. LED pins are already automatically set to the output direction,
-- and they cannot be set to the input direction. DIOx_P and DIOx_N are must set either output or input direction
-- before they can be used. When set to input direction, it is not allowed to write into these pins.
{#fun rp_DpinSetDirection as setPinDirection { `DigitalPin' , `PinDirection' } -> `RpError' #}
{#fun rp_DpinGetDirection as getPinDirection { `DigitalPin' , alloca- `PinDirection' peekEnum* } -> `RpError' #}

-- | Sets analog pins to default values. Output pins are set to 0 V.
{#fun rp_ApinReset as analogPinsReset  {} -> `RpError' #}

-- | Gets value from analog pin in volts.
{#fun rp_ApinGetValue as getPinValue { `AnalogPin' ,  alloca- `CFloat' peek* } -> `RpError' #}

-- | Gets raw value from analog pin.
{#fun rp_ApinGetValueRaw as getPinValueRaw { `AnalogPin' ,  alloca- `Integer' peekInt* } -> `RpError' #}

-- | Sets value in volts on analog output pin
{#fun rp_ApinSetValue as setPinValue { `AnalogPin' ,  `CFloat'} -> `RpError' #}

-- | Sets raw value on analog output pin.
{#fun rp_ApinSetValueRaw as setPinValueRaw { `AnalogPin' , `Int'} -> `RpError' #}

-- | Gets range in volts on specific pin
{#fun rp_ApinGetRange as getPinRange { `AnalogPin' ,  alloca- `CFloat' peek* ,  alloca- `CFloat' peek* } -> `RpError' #}

--------------------------------------------------------------------------------

-- | Sets the decimation used at acquiring signal. There is only a set of pre-defined decimation
{#fun rp_AcqSetDecimation as setDecimation  {`Decimation'} -> `RpError' #}
-- | Gets the decimation used at acquiring signal.
{#fun rp_AcqGetDecimation as getDecimation  {alloca- `Decimation' peekEnum*} -> `RpError' #}

-- | Gets the decimation factor used at acquiring signal in a numerical form.
{#fun rp_AcqGetDecimationFactor as getDecimationFactor {alloca- `Integer' peekInt*} -> `RpError' #}

-- | Sets the sampling rate for acquiring signal.
{#fun rp_AcqSetSamplingRate as setSamplingRate  {`SamplingRate'} -> `RpError' #}

-- | Gets the sampling rate for acquiring signal
{#fun rp_AcqGetSamplingRate as getSamplingRate { alloca- `SamplingRate' peekEnum* } -> `RpError' #}

--------------------------------------------------------------------------------
-- | Gets the sampling rate for acquiring signal in a numerical form in Hz. Although this method returns a float
-- value representing the current value of the sampling rate, there is only a set of pre-defined sampling rate
-- values which can be returned. 
{#fun rp_AcqGetSamplingRateHz as getSamplingRateHz { alloca- `CFloat' peek* } -> `RpError' #}

-- | Enables or disables averaging of data between samples.
-- Data between samples can be averaged by setting the averaging flag in the Data decimation register.
{#fun rp_AcqSetAveraging as enableAvaraging  {`Bool'} -> `RpError' #}

-- | Returns information if averaging of data between samples is enabled or disabled.
{#fun rp_AcqGetAveraging as getAvaraging  { alloca- `Bool' peekBool*} -> `RpError' #}

-- | Sets the trigger source used at acquiring signal. When acquiring is started,
-- the FPGA waits for the trigger condition on the specified source and when the condition is met, it
-- starts writing the signal to the buffer.
{#fun rp_AcqSetTriggerSrc as setTriggerSrc  {`TriggerSource'} -> `RpError' #}

-- | Gets the trigger source used at acquiring signal.
{#fun rp_AcqGetTriggerSrc as getTriggerSrc  {alloca- `TriggerSource' peekEnum*} -> `RpError' #}

-- |  Returns the trigger state. Either it is waiting for a trigger to happen, or it has already been triggered.
-- By default it is in the triggered state, which is treated the same as disabled.
{#fun rp_AcqGetTriggerState as getTriggerState  {alloca- `TriggerState' peekEnum*} -> `RpError' #}

-- | Sets the number of decimated data after trigger written into memory
{#fun rp_AcqSetTriggerDelay as setTriggerDelay  { `Int' } -> `RpError' #}

-- | Returns current number of decimated data after trigger written into memory.
{#fun rp_AcqGetTriggerDelay as getTriggerDelay  { alloca- `Integer' peekInt* } -> `RpError' #}

-- | Sets the amount of decimated data in nanoseconds after trigger written into memory.
{#fun rp_AcqSetTriggerDelayNs as setTriggerDelayNs  { fromIntegral `Integer'  } -> `RpError' #}

-- | Returns the current amount of decimated data in nanoseconds after trigger written into memory.
{#fun rp_AcqGetTriggerDelayNs as getTriggerDelayNs  { alloca- `Integer' peekInt* } -> `RpError' #}

-- | Sets the trigger threshold value in volts. Makes the trigger when ADC value crosses this value.
{#fun rp_AcqSetTriggerLevel as setTriggerLevel  { `CFloat' } -> `RpError' #}

-- | Gets currently set trigger threshold value in volts
{#fun rp_AcqGetTriggerLevel as getTriggerLevel  { alloca- `CFloat' peek* } -> `RpError' #}

-- |  Sets the trigger threshold hysteresis value in volts.
-- Value must be outside to enable the trigger again.
{#fun rp_AcqSetTriggerHyst as setTriggerLevelHyst  { `CFloat' } -> `RpError' #}

-- | Gets currently set trigger threshold hysteresis value in volts
{#fun rp_AcqGetTriggerHyst as getTriggerLevelHyst  { alloca- `CFloat' peek* } -> `RpError' #}

-- | Sets the acquire gain state. The gain should be set to the same value as it is set on the Red Pitaya
{#fun rp_AcqSetGain as setAcqGain { `Channel' , `PinState' } -> `RpError' #}

-- | Returns the currently set acquire gain state in the library. It may not be set to the same value as
-- it is set on the Red Pitaya hardware by the LV/HV gain jumpers. LV = 1V; HV = 20V.
{#fun rp_AcqGetGain as getAcqGain { `Channel' , alloca- `PinState' peekEnum* } -> `RpError' #}

-- | Returns the currently set acquire gain in the library. It may not be set to the same value as
-- it is set on the Red Pitaya hardware by the LV/HV gain jumpers. Returns value in Volts.
{#fun rp_AcqGetGainV as getAcqGainV { `Channel' , alloca- `CFloat' peek* } -> `RpError' #}

-- |  Returns current position of ADC write pointer.
{#fun rp_AcqGetWritePointer as getAcqWritePointer { alloca- `Integer' peekInt* } -> `RpError' #}

-- | Returns position of ADC write pointer at time when trigger arrived.
{#fun rp_AcqGetWritePointerAtTrig as getAcqWritePointerAtTrg { alloca- `Integer' peekInt* } -> `RpError' #}

-- | Starts the acquire. Signals coming from the input channels are acquired and written into memory.
{#fun rp_AcqStart as acqStart {} -> `RpError' #}

-- | Resets the acquire writing state machine.
{#fun rp_AcqReset as acqReset {} -> `RpError' #}


-- | Normalizes the ADC buffer position. Returns the modulo operation of ADC buffer size...
{#fun rp_AcqGetNormalizedDataPos as getNormalisedDataPosition { fromIntegral `Integer'  } ->  `Integer' fromIntegral #}



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
getAckDataRaw :: Channel -> Int -> Int -> IO ( RpError , [CShort])
getAckDataRaw = getPosSizeBufferHelper {#call rp_AcqGetDataRaw#}

-- | get  ADC data Voltage from channel passing position and size
getAckDataV :: Channel -> Int -> Int -> IO ( RpError , [CFloat])
getAckDataV = getPosSizeBufferHelper {#call rp_AcqGetDataV#}

-- | Returns the ADC buffer in raw units from the oldest sample to the newest one.
-- First parameter is maximal length of returned data (how large buffer to allocate)
getAckOldestDataRaw :: Int -> Channel ->  IO ( RpError , [CShort])
getAckOldestDataRaw = getSizeBufferHelper {#call rp_AcqGetOldestDataRaw#}

-- | Returns the ADC buffer in raw units from the latest
-- First parameter is maximal length of returned data (how large buffer to allocate)
getAckLatestDataRaw :: Int -> Channel ->  IO ( RpError , [CShort])
getAckLatestDataRaw = getSizeBufferHelper {#call rp_AcqGetLatestDataRaw#}

-- | Returns the ADC buffer in volts from the oldest sample to the newest one.
-- First parameter is maximal length of returned list (how large buffer to allocate)
getAckOldestDataV :: Int -> Channel ->  IO ( RpError , [CFloat])
getAckOldestDataV = getSizeBufferHelper {#call rp_AcqGetOldestDataV#}

-- | Returns the ADC buffer in volts from the latest
-- First parameter is maximal length of returned list (how large buffer to allocate)
getAckLatestDataV :: Int -> Channel ->  IO ( RpError , [CFloat])
getAckLatestDataV = getSizeBufferHelper {#call rp_AcqGetLatestDataV#}

{#fun rp_AcqGetBufSize  as getAcqBufSize { alloca- `Integer' peekInt* } -> `RpError' #}

-- |  Gets data about system health like temperature
{#fun rp_HealthGetValue as getHealthValue { `Health' ,  alloca- `CFloat' peek* } -> `RpError' #}

-- | Sets generate to default values.
{#fun rp_GenReset as generatorReset  {} -> `RpError' #}

-- | Enables output
{#fun rp_GenOutEnable as generatorOutEnable  {`Channel'} -> `RpError' #}

-- | Disables output
{#fun rp_GenOutDisable as generatorOutDisable  {`Channel'} -> `RpError' #}

-- | Gets value true if channel is enabled otherwise return false.
{#fun rp_GenOutIsEnabled as generatorOutIsEnabled  {`Channel', alloca- `Bool' peekBool*} -> `RpError' #}

-- | Sets channel signal peak to peak amplitude.
{#fun rp_GenAmp as generatorAmp {`Channel' , `CFloat' } -> `RpError' #}

-- | gets channel signal peak to peak amplitude
{#fun rp_GenGetAmp as getGeneratorAmp {`Channel' , alloca- `CFloat' peek* } -> `RpError' #}


-- | Sets DC offset of the signal. signal = signal + DC_offset.
{#fun rp_GenOffset as setGeneratorOffset {`Channel' , `CFloat' } -> `RpError' #}

-- | Sets channel signal frequency.
{#fun rp_GenFreq as generatorFrequency {`Channel' , `CFloat' } -> `RpError' #}

-- | Sets channel signal phase. This shifts the signal in time.
{#fun rp_GenPhase as generatorPhase {`Channel' , `CFloat' } -> `RpError' #}


-- | Sets channel signal phase. This shifts the signal in time.
{#fun rp_GenWaveform as generatorWaveForm {`Channel' , `Waveform' } -> `RpError' #}


setBufferHelper :: (Storable a,Integral i) => (Ptr a -> i -> IO r) -> [a] -> IO r
setBufferHelper fun xs = withArrayLen xs (\i p -> fun p (fromIntegral i))


-- | Sets user defined waveform
generatorArbWaveForm :: Channel -> [CFloat] -> IO RpError
generatorArbWaveForm c xs = (toEnum . fromIntegral) <$>
            setBufferHelper ({#call rp_GenArbWaveform #} 
            ((fromIntegral . fromEnum) c)) xs 


-- | Sets duty cycle of PWM signal.
{#fun rp_GenDutyCycle as generatorDutyCycle {`Channel' , `CFloat' } -> `RpError' #}

-- | Sets generation mode. 
{#fun rp_GenMode as generatorMode {`Channel' , `GeneratorMode' } -> `RpError' #}

-- | Sets number of generated waveforms in a burst. If -1 a continuous signal will be generated.
{#fun rp_GenBurstCount as generatorBursts {`Channel' , fromInteger `Integer' } -> `RpError' #}

-- | Sets number of burst repetitions. This determines how many bursts will be generated.
-- If -1 a continuous signal will be generated.
{#fun rp_GenBurstRepetitions as generatorBurstsRepetitions {`Channel' , fromInteger `Integer' } -> `RpError' #}


-- | Sets the time/period of one burst in micro seconds. Period must be equal or greater then the time of one burst.
-- If it is greater than the difference will be the delay between two consequential bursts.
{#fun rp_GenBurstPeriod as generatorBurstsPeriod {`Channel' , fromInteger `Integer' } -> `RpError' #}

-- | Sets trigger source.
{#fun rp_GenTriggerSource as generatorTriggerSource {`Channel' , `TriggerSource' } -> `RpError' #}
