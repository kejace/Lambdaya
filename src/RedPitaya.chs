
module RedPitaya (Dpin(..), PinState(..), PinDirection(..), AnalogPin(..),
        Health(..), Waveform(..), GeneratorMode(..), TriggerSource(..),
        Channel(..), SamplingRate(..), Decimation(..), TriggerSourceAcq(..),
        TriggerState(..), RpError(..),
        rpinit, release, getVersion, getError
    )
where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Marshal.Utils

import Data.Functor
import Control.Monad

#c
#include "rp.h"
#endc

{#enum rp_dpin_t as Dpin {underscoreToCase} with prefix = "RP_" deriving (Eq, Show) #}
{#enum rp_pinState_t as PinState {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}
{#enum rp_pinDirection_t as PinDirection {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}
{#enum rp_apin_t as AnalogPin {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}
{#enum rp_health_t as Health {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}
{#enum rp_waveform_t as Waveform {underscoreToCase} with prefix = "RP_WAVEFORM_" deriving (Eq, Show)#}
{#enum rp_gen_mode_t as GeneratorMode {underscoreToCase} with prefix = "RP_GEN_MODE_" deriving (Eq, Show)#}
{#enum rp_trig_src_t as TriggerSource {underscoreToCase} with prefix = "RP_GEN_TRIG_" deriving (Eq, Show)#}
{#enum rp_channel_t as Channel {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}
{#enum rp_acq_sampling_rate_t as SamplingRate {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}
{#enum rp_acq_decimation_t as Decimation {underscoreToCase} with prefix = "RP_" deriving (Eq, Show)#}
{#enum rp_acq_trig_src_t as TriggerSourceAcq {underscoreToCase} with prefix = "RP_TRIG_SRC_" deriving (Eq, Show)#}
{#enum rp_acq_trig_state_t as TriggerState {underscoreToCase} with prefix = "RP_TRIG_STATE_" deriving (Eq, Show)#}

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

{#fun rp_Init as rpinit  {} -> `RpError' #}
{#fun rp_Release as release  {} -> `RpError' #}
{#fun rp_GetVersion as getVersion  {} -> `String' #}
{#fun rp_GetError as getError  {`RpError'} -> `String' #}

{#fun rp_DpinReset as pinsReset  {} -> `RpError' #}
{#fun rp_DpinSetState as setPinState { `Dpin' , `PinState' } -> `RpError' #}
{#fun rp_DpinGetState as getPinState { `Dpin' ,  alloca- `PinState' peekEnum* } -> `RpError' #}
{#fun rp_DpinSetDirection as setPinDirection { `Dpin' , `PinDirection' } -> `RpError' #}
{#fun rp_DpinGetDirection as getPinDirection { `Dpin' , alloca- `PinDirection' peekEnum* } -> `RpError' #}

{#fun rp_ApinReset as analogPinsReset  {} -> `RpError' #}
{#fun rp_ApinGetValue as getPinValue { `Dpin' ,  alloca- `CFloat' peek* } -> `RpError' #}
{#fun rp_ApinGetValueRaw as getPinValueRaw { `Dpin' ,  alloca- `Integer' peekInt* } -> `RpError' #}
{#fun rp_ApinSetValue as setPinValue { `Dpin' ,  `CFloat'} -> `RpError' #}
{#fun rp_ApinSetValueRaw as setPinValueRaw { `Dpin' , `Int'} -> `RpError' #}
{#fun rp_ApinGetRange as getPinRange { `Dpin' ,  alloca- `CFloat' peek* ,  alloca- `CFloat' peek* } -> `RpError' #}

{#fun rp_AcqSetDecimation as setDecimation  {`Decimation'} -> `RpError' #}
{#fun rp_AcqGetDecimation as getDecimation  {alloca- `Decimation' peekEnum*} -> `RpError' #}
{#fun rp_AcqGetDecimationFactor as getDecimationValue  {alloca- `Integer' peekInt*} -> `RpError' #}
{#fun rp_AcqSetSamplingRate as setSamplingRate  {`SamplingRate'} -> `RpError' #}
{#fun rp_AcqGetSamplingRate as getSamplingRate { alloca- `SamplingRate' peekEnum* } -> `RpError' #}
{#fun rp_AcqGetSamplingRateHz as getSamplingRateHz { alloca- `CFloat' peek* } -> `RpError' #}
{#fun rp_AcqSetAveraging as setAvaraging  {`Bool'} -> `RpError' #}
{#fun rp_AcqGetAveraging as getAvaraging  { alloca- `Bool' peekBool*} -> `RpError' #}
{#fun rp_AcqSetTriggerSrc as setTriggerSrc  {`TriggerSource'} -> `RpError' #}
{#fun rp_AcqGetTriggerSrc as getTriggerSrc  {alloca- `TriggerSource' peekEnum*} -> `RpError' #}
{#fun rp_AcqGetTriggerState as getTriggerState  {alloca- `TriggerState' peekEnum*} -> `RpError' #}
{#fun rp_AcqSetTriggerDelay as setTriggerDelay  { `Int' } -> `RpError' #}
{#fun rp_AcqGetTriggerDelay as getTriggerDelay  { alloca- `Integer' peekInt* } -> `RpError' #}


{#fun rp_AcqSetTriggerDelayNs as setTriggerDelayNs  { fromIntegral `Integer'  } -> `RpError' #}
{#fun rp_AcqGetTriggerDelayNs as getTriggerDelayNs  { alloca- `Integer' peekInt* } -> `RpError' #}

{#fun rp_AcqSetTriggerLevel as setTriggerLevel  { `CFloat' } -> `RpError' #}
{#fun rp_AcqGetTriggerLevel as getTriggerLevel  { alloca- `CFloat' peek* } -> `RpError' #}



