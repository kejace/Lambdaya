import Control.Monad.State  

type FpgaM a = StateT Int IO a
-- workaround
type FpgaM2  = StateT Int IO

newtype Fpga a m = Fpga ( m a )

--getStateType :: Fpga a (StateT Int IO) -> FpgaM a --Fine
--getStateType :: Fpga a FpgaM  -> FpgaM a -- Compile Error
getStateType :: Fpga a FpgaM  -> FpgaM a --Fine (workaround)

getStateType (Fpga s) = s 


main = print ""
