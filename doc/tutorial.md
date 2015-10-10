tutorial for System.Readpitaya Libraray
-----------------------------------------------

System.Readpitaya is native haskell library for [redpitaya](http://redpitaya.com/) that
enables direct bindings on
[FPGA](https://github.com/RedPitaya/RedPitaya/blob/master/FPGA/release1/doc/RedPitaya_HDL_memory_map.odt?raw=true).
This library is aimed  to run on redpitaya and is inteded for use with crosscompiler.

To build and run code and examples you need cross-ghc compiler for armel processor.



Here it is  simpel an minimal example to use library

    import System.RedPitaya.Fpga

    main = withOpenFpga (setLed 0x55) 


This code imports necessary library. 3. is definition of main prgram 

withOpenFpga 

