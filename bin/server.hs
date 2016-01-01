
import System.RedPitaya.Fpga
import System.RedPitaya.Tcp
import System.Environment

main =  do 
    arg <- getArgs
    let port = fromInteger . read . head $ arg ++ ["4242"] 
    runRpServer port
