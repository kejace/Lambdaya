Lambdaya
--------

### Library for doing IO on [RedPitaya](http://redpitaya.com/) using [Haskell](https://www.haskell.org/)

This library provides native binding on [Fpga Registry map](https://github.com/RedPitaya/RedPitaya/blob/master/FPGA/release1/doc/RedPitaya_HDL_memory_map.odt?raw=true)

[Here](doc/build.mk) you can find notes on how to build GHC as crosscompiler
And [Here](doc/tutorial.mk) you can check for tutorial aimed for novices.


Here as short exemple is full featured hello world that tunns bunch of LEDs on RP

    import System.RedPitaya.Fpga

    main = withOpenFpga (setLed 0x55)
