-- This program creates a qubit |0> and comments "<| Hello Quantum World! |>".
-- The graph of circuit output as a PDF file, which will automatically open.
-- 
-- ©2017 qWalker All Right Reserved
-- ===================================================================================

import Quipper

hello_quantum_world :: Bool -> Circ Qubit
hello_quantum_world var = do
  qbit <- qinit var
  label (qbit) ("<| Hello Quantum World! |>")
  return qbit

main = print_simple Preview (hello_quantum_world False)
