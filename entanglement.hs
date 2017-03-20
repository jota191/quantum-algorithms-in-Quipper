-- This program create the entanglement circuit and simulate it 10 times.
-- The graph of circuit output as a PDF file, which will automatically be opened.
-- The result for 10 simulations are showed in command line, which are either
-- (True, True) or (False, False).
-- 
-- ©2017 qWalker All Right Reserved
-- ===================================================================================

import Quipper
-- |  import modules for circuits simulations
import QuipperLib.Simulation
import System.Random
-- | For using replicateM_ to do 10 simulations
import Control.Monad

entanglement :: Bool -> Bool  -> Circ (Bit, Bit)
entanglement var1 var2 = do
  -- initialize control qubit and target qubit
  cntrl_qbit <- qinit var1
  trget_qbit <- qinit var2

  -- apply hadamard gate and C-not gate
  hadamard_at cntrl_qbit
  qnot_at trget_qbit `controlled` cntrl_qbit

  -- measurement at both qubits
  (cntrl_qbit, trget_qbit) <- measure (cntrl_qbit, trget_qbit)
  return (cntrl_qbit, trget_qbit)

main = do
  print_generic Preview (entanglement False False)
  replicateM_ 10 $
    do
      g <- newStdGen
      print $ run_generic g (0.0 :: Double) (entanglement False False)
