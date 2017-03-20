-- This program approximate two Vk gates for linear combinations of unitary
-- transformation within precision 10^(-10). The second one is transpose
-- conjugate to the first one, which will be used at the final step, while
-- the first one will be used at the first step.
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
-- Here I chose the k in Vk as sqrt(9/10), which were calculated using other
-- calculator, and then wrote in the matrix representation. This matrix
-- representation was then used to approximate  synthesis the single qubit 
-- unitary gate using the embedded function in Quipper(approximate_synthesis_u2).
-- From the calculation of LCU algorithm we know that the failure probability is
-- less than 0.36.
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
-- | import module to approximate synthesis
import QuipperLib.Synthesis
import Quantum.Synthesis.Ring
import Quantum.Synthesis.Matrix
import System.Random

-- | define the first Vk gate
linear_combination_circuit :: IO ()
linear_combination_circuit = do
  -- get a random number
  g1 <- newStdGen
  let circ1 = approximate_synthesis_u2 True prec op1 g1
  -- call linear_combination_circuit2
  linear_combination_circuit2
  -- print the circuit for Vk1 in pdf
  print_simple Preview circ1
  where
  prec = 10 * digits
  op1 = matrix2x2 (0.9486832980505138, -0.3162277660168379)
                  (0.3162277660168379,  0.9486832980505138)
  
-- | define the final Vk gate
linear_combination_circuit2 :: IO ()
linear_combination_circuit2 = do
  -- get a random number
  g2 <- newStdGen
  let circ2 = approximate_synthesis_u2 True prec op2 g2
  -- print the circuit for Vk2 in pdf
  print_simple Preview circ2
  where
  prec = 10 * digits
  op2 = matrix2x2 (0.9486832980505138,  0.3162277660168379)
                  (-0.3162277660168379, 0.9486832980505138)

-- | main funciton
main = linear_combination_circuit
