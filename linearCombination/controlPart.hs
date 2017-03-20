-- This program implements the gates in control part for linear combinations of 
-- unitary transformation.
-- 
-- The algorithm of Linear Combinations of Unitary(LCU) was invented by Childs
-- et.al, which was used to simulate the evolution of quantum system more
-- efficiently. It implements the linear combination of unitary operators
-- (kU1+U2) with a small probability fail. This method can be generalized to
-- implement (k1U1+k2U2+...+knUn).
--
-- It was further applied to HHL algorithm, which achieveed exponential speed-up
-- in error. Therefore, this algorithm can be  used to solve linear equation
-- system more efficiently than any other methods, thus it has a very wide
-- application in physics, since physicists often encounter solving linear
-- equation system, like Schodinger equation.
-- 
-- It is a pity that I don't know how to connect these Vk gates and gates
-- in controlPart.hs together, so I can't have a whole program for LCU circuit.
-- Addtionally, even though I have the whole circuit, I didn'd find a way to 
-- simulate this circuit using the simulator inside Quipper(QuipperLib.Simulation).
-- :(
-- 
-- The description of LCU can be found at arXiv:1202.5822v1. The application
-- of LCU to linear system equation can be found at arXiv:1511.02306.
-- 
-- ©2017 qWalker All Right Reserved
-- =============================================================================

import Quipper
-- | import modules for circuits simulation
import QuipperLib.Synthesis
import Quantum.Synthesis.Ring
import Quantum.Synthesis.Matrix
import System.Random


controlPart :: Circ Bit
controlPart = do
  comment "Control Part"
  top_qubit <- qinit False
  bottom_qubit <- qinit False
  gate_X_at top_qubit
  qnot_at bottom_qubit `controlled` top_qubit
  gate_X_at top_qubit
  qnot_at bottom_qubit `controlled` top_qubit
  top_qubit <- measure(top_qubit)
  return top_qubit

main = print_simple Preview controlPart
