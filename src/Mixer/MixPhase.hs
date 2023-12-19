-- | Provides data formats for fmailies of Hamiltonian matrices.

module Quoptimize.Mixer.MixPhase where

-------------------------------------------------------------------------------
-- * Import Section.

import Quipper

import Quoptimize.Mixer.Init
-------------------------------------------------------------------------------
-- * Interface to mixer.

-- | Takes as input a state and applies the mixer gates on it.
mixerPhase :: [Double] -> [Qubit] -> Circ ()
mixerPhase betas qs = do
  qs <- hGate qs 
  sequence_ [expZt_at θ x | (θ,x) <- zip betas qs]
  qs <- hGate qs
  return () 
--  qs >>= hGate 
--  qs >>= sequence [expZt θ | θ <- betas]
--  qs >>= hGate 

mixerP = case (initialization 7) of
            Just circ -> print_generic Preview (circ >>= (mixerPhase (replicate 7 3) ))
            Nothing -> print "Nothing"
            

