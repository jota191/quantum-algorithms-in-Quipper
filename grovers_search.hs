-- This program contains a circuit of Grovers' algorithm and simulate
-- it with a 5-qubit oracle.
-- 
-- The program contains three major parts:
-- 1. generate Grovers' circuit.
-- 2. implement a 5-qubit oracle.
-- 3. simulate the circuit with this oracle.
-- 
-- The number of oracles we need to successfully run Grover' search is
-- ceiling(pi*sqrt(N)/4). (not query complexity sqrt(N))
-- 
-- A graph of circuit output as a PDF file, which will automatically be opened.
-- The results for simulation will be showed on command line, which are 5
-- boolean values that indicate the position of desired object. In this case, 
-- [True, False, True, True, True] corresponding to 2^0+2^2+2^3+2^4 = 29,
-- which means that the object we looking for is located at 29 among 32 positions.
-- 
-- This program is firstly coded by authors of arXiv:1406.4481v2, and then 
-- modified by qWalker.
-- ©2017 qWalker All Right Reserved
-- =============================================================================

import Quipper
-- | import modules for circuits simulation
import qualified Data.Map as Map
import QuipperLib.Simulation
import System.Random

-- | Generate Circuits============================================================
-- | define Oracle data type
data Oracle = Oracle {
  qubit_num :: Int,
  function :: ([Qubit], Qubit) -> Circ ([Qubit], Qubit)
}

-- | define phase_inversion function
phase_inversion ::(([Qubit],Qubit)->Circ([Qubit],Qubit))->([Qubit],Qubit)-> Circ([Qubit],Qubit)
phase_inversion oracle (top_qubits, bottom_qubit) = do
  comment "start phase inversion"
  -- call oracle
  oracle (top_qubits, bottom_qubit)
  comment "end phase inversion"
  return (top_qubits, bottom_qubit)

-- | define inversion_about_mean function
inversion_about_mean :: ([Qubit], Qubit) -> Circ ([Qubit], Qubit)
inversion_about_mean (top_qubits, bottom_qubit) = do
  comment "start inversion about mean"
  -- apply X_gate at top qubit
  mapUnary gate_X top_qubits
  -- separate target and control qubits
  let pos = (length top_qubits) - 1
  let target_qubit = top_qubits !! pos
  let controlled_qubit = take pos top_qubits
  -- apply hadamard at target_qubit
  hadamard_at target_qubit
  -- apply qnot gate at target qubit
  qnot_at target_qubit `controlled` controlled_qubit
  -- apply hadamard again at top
  hadamard_at target_qubit
  -- apply X_gate at bottom
  mapUnary gate_X top_qubits
  comment "end inversion about mean"
  return (top_qubits, bottom_qubit)

-- | define grover_search_circuit function
grover_search_circuit :: Oracle -> Circ ([Bit])
grover_search_circuit oracle = do
  comment "Grover Search algorithm"
  -- set the value of n
  let n = toEnum (qubit_num oracle) :: Float
  -- set the index number to iterate (pi/4)*√(2^n) times
  let index = (ceiling(pi /4 * sqrt (2**n))) :: Int
  -- create the ancillaes
  top <- qinit (replicate (qubit_num oracle) False)
  bottom <- qinit True
  label (top, bottom) ("|0>","|1>")
  -- apply hadamard gate at string
  mapUnary hadamard top
  hadamard_at bottom
  -- start to iterate
  for 1 (index) 1 $ \i -> do
    comment "start grover iteration"
    -- call phase inversion
    (top, bottom) <- phase_inversion (function oracle) (top, bottom)
    -- call inversion about mean
    (top, bottom) <- inversion_about_mean (top, bottom)
    comment "after grover iteration"
  endfor
  -- measure qubit string and return result
  hadamard_at bottom
  (top, bottom) <- measure (top, bottom)
  cdiscard bottom
  return (top)


-- | Implement Five Qubits Oracle=========================================================
-- | declare 5_qubit_oracle data type
n_qubit_oracle :: Oracle
n_qubit_oracle = Oracle {
  qubit_num = 5,
  function = n_qubit_oracle_function
}
-- | initialize n_qubit_oracle’s function
n_qubit_oracle_function :: ([Qubit],Qubit) -> Circ ([Qubit],Qubit)
n_qubit_oracle_function (controlled_qubit, target_qubit) = do
  qnot_at target_qubit `controlled` controlled_qubit .==. [1,0,1,1,1]
  return (controlled_qubit, target_qubit)


-- | Simulate Grovers' circuits ==========================================================
-- | define simulate function
simulate :: Circ [Bit] -> [Bool]
simulate oracle = reverse (map not (run_generic (mkStdGen 1) (1.0::Float) oracle))

-- | define circuit function
circuit :: (Circ [Bit] -> [Bool]) -> Oracle -> IO ()
circuit run oracle =
-- first Grovers will apply on oracle
-- then run function will evaluate the result
  print (run (grover_search_circuit oracle))

-- | main function
main = do
  print_generic Preview (grover_search_circuit n_qubit_oracle)
  circuit simulate n_qubit_oracle
