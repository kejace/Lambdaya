

-- | 
module System.RedPitaya.Tutorial (
{-| 
== tutorial for System.Readpitaya Libraray
#tutorial-for-system.readpitaya-libraray#

System.Readpitaya is native haskell library for
<http://redpitaya.com/ redpitaya> that enables direct bindings on
<https://github.com/RedPitaya/RedPitaya/blob/master/FPGA/release1/doc/RedPitaya_HDL_memory_map.odt?raw=true FPGA>.
This library is aimed to run on redpitaya and is inteded for use with
crosscompiler.

To build and run code and examples you need cross-ghc compiler for armel
processor.

Here it is simpel an minimal example to use library

> import System.RedPitaya.Fpga
>
> main = withOpenFpga (setLed 0x55) 

withOpenFpga is function, that takes Fpga operation and executes it. It
handels opening and closing of FPGA resources so it is advisable that
that all operation are executed inside and @withOpenFpga@ gets called
only once.

If you take look of prototype setLed you can see that it has protype
@setLed :: Registry -> Fpga ()@ And that can be understand that it takes
@Registy@ (that is 32 bit unsigned integer) and returns @Fpga ()@.
@Fpga a@ is type that can run in @withOpenFpga@ function. @()@ is empty
or void value that is retured after @setLed@ gets called. What @setLed@
does it takes writes registry value at apropriate addres in fpga
registry map described in document FPGA metioned in first section.

In our next example we will build program that displays value that is
written in this registry using function @getLed@

> import System.RedPitaya.Fpga
> import Numeric (showHex)
>
> main = do
>     reg <- withOpenFpga getLed
>     putStrLn ( show reg)

here @reg@ is value can get \"get\" from @withOpenFpga@. Function
@withOpenFpga@ has prototype of \'Fpga a -> IO a\'. That mean that
returns same value as it is returned from executing last command
executed in block. In our case that is just function @getLed@. Since
@getLed :: Fpga Registry@ that mean that ir returns @Fpga Registry@
type.

@IO Registry@ (@a@ in definition of @withOpenFpga@ becomes @Registry@).
We haven\'t mentioned @IO a@ before but that is type one can execute
commands like @putStrLn@ with.

> import System.RedPitaya.Fpga
> import Numeric (showHex)
>
> main = do
>     reg <- withOpenFpga ( do
>                             setLed 0x44
>                             r <- getLed
>                             return r
>                         )
>     putStrLn ( show reg )

This code runs two @do@ blocks. One handles IO type where types @IO a@
are running and other do block with type \'Fpga a\'. If one needs to run
IO types inside Fpga block he\/she needs to change IO type to Fpga type
using function liftIO.

> import System.RedPitaya.Fpga
> import Numeric (showHex)
> import Control.Monad.IO.Class (liftIO)
>
> main = do
>     withOpenFpga $ do
>             setLed 0x44
>             reg <- getLed 
>             liftIO $ putStrLn $ show reg
>     putStrLn "done"

In this example we also use build in operator @$@ that works as a
brackets to to the end.

> liftIO $ putStrLn $ showHex reg "" 
> -- is same as
> liftIO ( putStrLn ( showHex reg "" ))

If you like to know more about operator @$@ or any other standard
functions used, you can use invaluable search engine
<https://www.haskell.org/hoogle/?hoogle=%24 Hoogle> where is link on
haddock manuals with description and link to source code. Similar search
engines are also availabe at <http://hayoo.fh-wedel.de/ Hayoo!> and
<https://www.stackage.org/ Stackage>.

Lets do some more tricks with diodes and using standard

> import System.RedPitaya.Fpga
> import Control.Concurrent (threadDelay)
> import Data.Bits
> import Control.Monad.IO.Class (liftIO)
>
> knightRider n = do
>     let ledn = 1 + abs ((mod n 12) - 6) -- 6,5,4,3,2,1,2,3,4,5,6,7,6,5,4,3,2,1,2,3,4,5,6,7,6,5...
>     setLed ( shiftL 1 ledn )
>     liftIO ( threadDelay (30000) )
>     knightRider (n + 1)
>
> main = withOpenFpga (knightRider 0)

@knightRider@ is function that gets caled each time with next
integer.@ledn@ in generated in way that provides numbering in apropriate
order. shiftL function . Next @threadDelay@ delays execution, by 30000
microseconds and since @nightRider@ runs inside @withOpenFpga@ we have
to use @liftIO@ just to make type compatible. Finaly we call
@nightRider@ with next number and whole procees repeats.

So far we covered lot if of stuff on how to use this library. Even
though all examples were reading and writing to led registry you can be
sure that all function provided by library that has type signature
@Registry -> Fpga ()@ works and can be used just like @setLed@ and all
functions with type signature @Fpga Registry@ works like getLed.

-} 
)
where
import System.RedPitaya.Fpga
