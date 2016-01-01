

-- | 
module System.RedPitaya.Tutorial (
{-| 
== tutorial for System.Readpitaya Libraray
#tutorial-for-system.readpitaya-libraray#

System.Readpitaya native haskell library for
<http://redpitaya.com/ redpitaya> that enables direct bindings on
<https://github.com/RedPitaya/RedPitaya/blob/master/fpga/doc/RedPitaya_HDL_memory_map.odt?raw=true FPGA>.
It also supports remote redpitaya contolling trough similar inteface.

Here it is simple an minimal example to use library

> import System.RedPitaya.Fpga
> import System.RedPitaya.Tcp
>
> rpIp = "10.42.0.219"
> rpPort = 4242
>
> main = runRemoteRp rpIp rpPort (setLed 0x55) 

@runRemoteRp@ is function that execute @FpgaSetGet@ over network

Lambdaya libraray enables also execution of code natively trough
@withOpenFpga@ using same @FpgaSetGet@ action

> import System.RedPitaya.Fpga
> import System.RedPitaya.Arm
>
> main = withOpenFpga (setLed 0x55) 

Inspiration for last example is technology from 1970s
https:\/\/www.youtube.com\/watch?v=Hm3AFz4wrw4

> import System.RedPitaya.Fpga
> import Control.Concurrent (threadDelay)
> import Data.Bits
> import Control.Monad.IO.Class (liftIO)
>
> knightRider n = do
>     let ledn = 1 + abs ((mod n 12) - 6)
>     setLed ( shiftL 1 ledn )
>     liftIO ( threadDelay (30000) )
>     knightRider (n + 1)
>
> rpIp = "10.42.0.219"
> rpPort = 4242
>
> main = runRemoteRp rpIp rpPort (knightRider 0)


-} 
)
where
import System.RedPitaya.Fpga
