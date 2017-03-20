-- This program  contains the implementation of Deutsch algorithm and complete
-- both constant and balanced oracles.
-- 
-- The program contains three major parts:
-- 1. generate Deutsch circuit.
-- 2. implement 2 constant oracles and 2 balanced oracles.
-- 3. simulate the circuit 4 times.
-- 
-- Four graphs of circuits output as PDF files, which will automatically be opened.
-- The results for simulation will be showed on command line, which are either
-- "Given oracle is Balanced." or "Given oracle is Constant.".
-- 
-- This program is firstly coded by authors of arXiv:1406.4481v2, and then modified
-- by qWalker.
-- ©2017 qWalker All Right Reserved
-- ===================================================================================


import Quipper
-- | import modules for circuits simulations
import qualified Data.Map as Map
import QuipperLib.Simulation
import System.Random

-- | define Oracle data type
data Oracle = Oracle{
  -- | oracle of function f(x)
  oracle_function :: (Qubit,Qubit) -> Circ (Qubit,Qubit)
}


-- | Generate Deutsch circuit ========================================================
-- | define deutsch_circuit function
deutsch_circuit :: Oracle -> Circ Bit
deutsch_circuit oracle = do
  -- create the qubits
  top_qubit <- qinit False
  bottom_qubit <- qinit True
  label (top_qubit, bottom_qubit) ("|0>","|1>")
  -- do the first Hadamards
  hadamard_at top_qubit
  hadamard_at bottom_qubit
  comment "before oracle"
  -- call the oracle
  oracle_function oracle (top_qubit, bottom_qubit)
  comment "after oracle"
  -- do the last Hadamards
  hadamard_at top_qubit
  -- measure qubits
  (top_qubit, bottom_qubit) <- measure (top_qubit, bottom_qubit)
  -- discard un-necessary output and return the result
  cdiscard bottom_qubit
  return top_qubit


-- | Implement 4 oracles =============================================================
-- | define constant_oracle_1’s data type
constant_oracle_1 :: Oracle
constant_oracle_1 = Oracle {
  oracle_function = constant_oracle_function_1
}
-- | initialize oracle function f(x)
constant_oracle_function_1 :: (Qubit,Qubit) -> Circ (Qubit,Qubit)
constant_oracle_function_1 (x,y) = do
  -- f(0) = 0; f(1) = 0
  return (x,y)

-- | define constant_oracle_2’s data type
constant_oracle_2 :: Oracle
constant_oracle_2 = Oracle {
  oracle_function = constant_oracle_function_2
}
-- | initialize oracle function f(x)
constant_oracle_function_2 :: (Qubit,Qubit) -> Circ (Qubit,Qubit)
constant_oracle_function_2 (x,y) = do
  -- f(0) = 1; f(1) = 1
  qnot_at y
  return (x,y)

-- | define balanced_oracle_1’s data type
balanced_oracle_1 :: Oracle
balanced_oracle_1 = Oracle {
  oracle_function = balanced_oracle_function_1
}
-- | initialize oracle function f(x)
balanced_oracle_function_1 :: (Qubit,Qubit) -> Circ (Qubit,Qubit)
balanced_oracle_function_1 (x,y) = do
  -- f(0) = 1; f(1) = 0
  qnot_at y `controlled` x
  return (x,y)

-- | define balanced_oracle_2’s data type
balanced_oracle_2 :: Oracle
balanced_oracle_2 = Oracle {
  oracle_function = balanced_oracle_function_2
}
-- | initialize oracle function f(x)
balanced_oracle_function_2 :: (Qubit,Qubit) -> Circ (Qubit,Qubit)
balanced_oracle_function_2 (x,y) = do
  -- f(0) = 0; f(1) = 1
  qnot_at x
  qnot_at y `controlled` x
  return (x,y)


-- | Simulate Deutsch circuit ==========================================================
-- | define simulate function
simulate :: Circ Bit -> Bool
simulate oracle = (run_generic (mkStdGen 1) (1.0::Double) oracle)

-- | define circuit function
circuit :: (Circ Bit -> Bool) -> Oracle -> IO ()
circuit run oracle =
  -- firstly deutsch_circuit will apply on oracle
  -- then run function will evaluate the result
  if run (deutsch_circuit oracle)
  then putStrLn "Given oracle is Balanced."
  else putStrLn "Given oracle is Constant."

-- | main function  
main = do
  -- | test constant_oracle_1
  print_generic Preview (deutsch_circuit constant_oracle_1)
  circuit simulate constant_oracle_1

  -- | test constant_oracle_2
  print_generic Preview (deutsch_circuit constant_oracle_2)
  circuit simulate constant_oracle_2
  
  -- | test balanced_oracle_1
  print_generic Preview (deutsch_circuit balanced_oracle_1)
  circuit simulate balanced_oracle_1

  -- | test balanced_oracle_2
  print_generic Preview (deutsch_circuit balanced_oracle_2)
  circuit simulate balanced_oracle_2
