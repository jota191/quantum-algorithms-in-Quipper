-- This program  contains the implementation of Deutsch-Jozsa algorithm and complete
-- both constant and balanced oracles for two qubit input.
-- 
-- The program contains three major parts:
-- 1. generate Deutsch_Jozsa circuit.
-- 2. implement 2 constant oracles and 6 balanced oracles.
-- 3. simulate the circuit 8 times.
-- 
-- Eight graphs of circuits output as PDF files, which will automatically be opened.
-- The results for simulation will be showed on command line, which are either
-- "Given oracle is Balanced." or "Given oracle is Constant.".
-- 
--
-- This program is firstly coded by authors of arXiv:1406.4481v2, and then modified 
-- by qWalker.
-- ©2017 qWalker All Right Reserved
-- ===================================================================================


import Quipper
-- | import modules for circuits simulation
import qualified Data.Map as Map
import QuipperLib.Simulation
import System.Random

-- | Generate Deutsch-Jozsa circuit ====================================================
-- | define modified Oracle data type
data Oracle = Oracle{
  -- | define the length of a string
  qubit_num :: Int,
  -- | define oracle function f(x)
  function :: ([Qubit], Qubit) -> Circ ([Qubit], Qubit)
}

-- | define deutsch_jozsa_circuit function
deutsch_jozsa_circuit :: Oracle -> Circ [Bit]
deutsch_jozsa_circuit oracle = do
  -- initialize string of qubits
  top_qubits <- qinit (replicate (qubit_num oracle) False)
  bottom_qubit <- qinit True
  label (top_qubits, bottom_qubit) ("|0>","|1>")
  -- do the first hadamard
  mapUnary hadamard top_qubits
  hadamard_at bottom_qubit
  comment "before oracle"
  -- call oracle
  function oracle (top_qubits, bottom_qubit)
  comment "after oracle"
  -- do the last hadamard
  mapUnary hadamard top_qubits
  -- measure qubits
  (top_qubits, bottom_qubit) <- measure (top_qubits, bottom_qubit)
  -- discard unnecessary output and return result
  cdiscard bottom_qubit
  return top_qubits


-- | Two qubit oracles ==============================================================
-- | define constant_oracle_1’s data type
constant_oracle_1 :: Oracle
constant_oracle_1 = Oracle {
  qubit_num = 2,
  function = constant_oracle_function_1
}
-- | initialize constant_oracle_1 function f(x)
constant_oracle_function_1 :: ([Qubit],Qubit) -> Circ ([Qubit],Qubit)
constant_oracle_function_1 (ins,out) = do
  -- f(00)=f(01)=f(10)=f(11)=0
  return (ins, out)

-- | define constant_oracle_2’s data type
constant_oracle_2 :: Oracle
constant_oracle_2 = Oracle {
  qubit_num = 2,
  function = constant_oracle_function_2
}
-- | initialize constant_oracle_2 function f(x)
constant_oracle_function_2 :: ([Qubit],Qubit) -> Circ ([Qubit],Qubit)
constant_oracle_function_2 (ins,out) = do
  -- f(00)=f(01)=f(10)=f(11)=1
  qnot_at out
  return (ins, out)

-- | define balanced_oracle_1’s data type
balanced_oracle_1 :: Oracle
balanced_oracle_1 = Oracle {
  qubit_num = 2,
  function = balanced_oracle_function_1
}
-- | initialize balanced_oracle_1 function f(x)
balanced_oracle_function_1 :: ([Qubit],Qubit) -> Circ ([Qubit],Qubit)
balanced_oracle_function_1 ([x,y],out) = do
  -- f(00)=f(11)=0; f(01)=f(10)=1
  qnot_at out `controlled` x
  qnot_at out `controlled` y
  return ([x,y],out)
balanced_oracle_function_1 _ = error "undefined" -- fallback case

-- | define balanced_oracle_2’s data type
balanced_oracle_2 :: Oracle
balanced_oracle_2 = Oracle {
  qubit_num = 2,
  function = balanced_oracle_function_2
}
-- | initialize balanced_oracle_2 function f(x)
balanced_oracle_function_2 :: ([Qubit],Qubit) -> Circ ([Qubit],Qubit)
balanced_oracle_function_2 ([x,y],out) = do
  -- f(01)=f(10)=0; f(00)=f(11)=1
  qnot_at out `controlled` x
  qnot_at out `controlled` y
  qnot_at out
  return ([x,y],out)
balanced_oracle_function_2 _ = error "undefined" -- fallback case

-- | define balanced_oracle_3’s data type
balanced_oracle_3 :: Oracle
balanced_oracle_3 = Oracle {
  qubit_num = 2,
  function = balanced_oracle_function_3
}
-- | initialize balanced_oracle_3 function f(x)
balanced_oracle_function_3 :: ([Qubit],Qubit) -> Circ ([Qubit],Qubit)
balanced_oracle_function_3 ([x,y],out) = do
  -- f(00)=f(01)=0; f(10)=f(11)=1
  qnot_at out `controlled` x
  return ([x,y],out)
balanced_oracle_function_3 _ = error "undefined" -- fallback case

-- | define balanced_oracle_4’s data type
balanced_oracle_4 :: Oracle
balanced_oracle_4 = Oracle {
  qubit_num = 2,
  function = balanced_oracle_function_4
}
-- | initialize balanced_oracle_4 function f(x)
balanced_oracle_function_4 :: ([Qubit],Qubit) -> Circ ([Qubit],Qubit)
balanced_oracle_function_4 ([x,y],out) = do
  -- f(10)=f(11)=0; f(00)=f(01)=1
  qnot_at out `controlled` x
  qnot_at out
  return ([x,y],out)
balanced_oracle_function_4 _ = error "undefined" -- fallback case

-- | define balanced_oracle_5’s data type
balanced_oracle_5 :: Oracle
balanced_oracle_5 = Oracle {
  qubit_num = 2,
  function = balanced_oracle_function_5
}
-- | initialize balanced_oracle_5 function f(x)
balanced_oracle_function_5 :: ([Qubit],Qubit) -> Circ ([Qubit],Qubit)
balanced_oracle_function_5 ([x,y],out) = do
  -- f(00)=f(01)=0; f(10)=f(11)=1
  qnot_at out `controlled` y
  return ([x,y],out)
balanced_oracle_function_5 _ = error "undefined" -- fallback case

-- | define balanced_oracle_6’s data type
balanced_oracle_6 :: Oracle
balanced_oracle_6 = Oracle {
  qubit_num = 2,
  function = balanced_oracle_function_6
}
-- | initialize balanced_oracle_6 function f(x)
balanced_oracle_function_6 :: ([Qubit],Qubit) -> Circ ([Qubit],Qubit)
balanced_oracle_function_6 ([x,y],out) = do
 -- f(10)=f(11)=0; f(00)=f(01)=1
  qnot_at out `controlled` y
  qnot_at out
  return ([x,y],out)
balanced_oracle_function_6 _ = error "undefined" -- fallback case


-- | Simulate Deutsch-Jozsa circuits ===================================================
-- | define simulate function
simulate :: Circ [Bit] -> Bool
simulate oracle = and (map not (run_generic (mkStdGen 1) (1.0::Float) oracle))

-- | define circuit function
circuit :: (Circ [Bit] -> Bool) -> Oracle -> IO ()
circuit run oracle =
-- firstly deutsch_jozsa will apply on oracle
-- then run function will evaluate the result
  if run (deutsch_jozsa_circuit oracle)
  then putStrLn "Given oracle is Constant."
  else putStrLn "Given oracle is Balanced."

-- | main function
main = do
  -- | test constant_oracle_1
  print_generic Preview (deutsch_jozsa_circuit constant_oracle_1)
  circuit simulate constant_oracle_1

  -- | test constant_oracle_2
  print_generic Preview (deutsch_jozsa_circuit constant_oracle_2)
  circuit simulate constant_oracle_2
  
  -- | test balanced_oracle_1
  print_generic Preview (deutsch_jozsa_circuit balanced_oracle_1)
  circuit simulate balanced_oracle_1

  -- | test balanced_oracle_2
  print_generic Preview (deutsch_jozsa_circuit balanced_oracle_2)
  circuit simulate balanced_oracle_2

  -- | test balanced_oracle_3
  print_generic Preview (deutsch_jozsa_circuit balanced_oracle_3)
  circuit simulate balanced_oracle_3

  -- | test balanced_oracle_4
  print_generic Preview (deutsch_jozsa_circuit balanced_oracle_4)
  circuit simulate balanced_oracle_4

  -- | test balanced_oracle_5
  print_generic Preview (deutsch_jozsa_circuit balanced_oracle_5)
  circuit simulate balanced_oracle_5

  -- | test balanced_oracle_6
  print_generic Preview (deutsch_jozsa_circuit balanced_oracle_6)
  circuit simulate balanced_oracle_6
  
