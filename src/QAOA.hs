-- | Provides data formats for fmailies of Hamiltonian matrices.

module Quoptimize.QAOA where

-------------------------------------------------------------------------------
-- * Import Section.
import Data.Maybe

import Quipper
import Quipper.Libraries.Decompose

import Quoptimize.Mixer.Init
import Quoptimize.Mixer.MixPhase
import Quoptimize.Cost.CostPhase
import Quoptimize.Hamiltonian.Format
-------------------------------------------------------------------------------
-- * Interface to mixer.

-- | Takes as input a state and applies the mixer gates on it.
qaoa :: Int -> [Double] -> QDiagHam -> [Qubit] -> Circ ()
qaoa n betas hamil bs = 
  let qs = initialization n
      f = mixerPhase betas
      c = trotter_step False 1 hamil bs
  in  case qs of 
        Just circ -> (circ >>= c)
        Nothing -> error "init"




main_singletons :: IO ()
main_singletons = do
  let zero3 = fromJust $ zeroQDiagHam 3
  let h1 = fromJust $ setComp 0 5 zero3
  let bs = replicate 3 qubit
  let circuit = qaoa 3 (replicate 3 3) h1 bs
  print_generic Preview circuit
  --let circuit' = decompose_generic Logical circuit
  {-case circuit of 
        Just circ -> print_generic Preview circ
        Nothing -> putStrLn "Nothing"-}





