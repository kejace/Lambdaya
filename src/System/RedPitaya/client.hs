
import Fpga2
import Tcp

main =  runRemoteRp "127.0.0.1" 4242 $ setLed 5 
