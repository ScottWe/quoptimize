-- | The initial phase for QAOA.

module Quoptimize.Mixer.Init where

-------------------------------------------------------------------------------
-- * Import Section.

import Quipper
import Quipper.Internal.Circuit


-------------------------------------------------------------------------------
-- * Creating the zero state.

-- | Takes as input the dim of a vector (i.e., a positive integer). Returns a
-- vector in state zero of the given dim.
zeroVec :: Int -> Maybe (Circ [Qubit])
zeroVec n 
  | n>0 = Just $ qinit (take n (repeat False))
  | otherwise = Nothing


-------------------------------------------------------------------------------
-- * Applying X-gates to the zero vector.

-- | Takes as input a dimension and returns X^ |0>^
hGate :: [Qubit] -> Circ [Qubit]
hGate qs = map_hadamard qs

initialization :: Int -> Maybe (Circ [Qubit])
initialization n =
  if n>0 
  then do 
    case zeroVec n of
        Just vec -> Just $ (vec >>= hGate)
        Nothing  -> Nothing
  else Nothing 

initM = case (initialization 7) of
            Just circ -> (print_generic Preview circ)
            Nothing -> print "Nothing" 
         
