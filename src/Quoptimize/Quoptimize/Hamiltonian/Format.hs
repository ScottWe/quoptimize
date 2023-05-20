-- | Provides data formats for fmailies of Hamiltonian matrices.

module Quoptimize.Hamiltonian.Format
  ( QDiagHam
  , dim
  , getComp
  , setComp
  , trace
  , zeroQDiagHam
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.IntMap as IntMap

-------------------------------------------------------------------------------
-- * Constructors and Destructors for a Diagonal Hamiltonian encoding.

-- | Data format for a diagonal Hamiltonian.
data QDiagHam = QDiagHam { dim   :: Int
                         , trace :: Rational
                         , diag  :: IntMap.IntMap Rational
                         } deriving (Show, Eq)

-- | Takes as input the size of a matrix (i.e., a positive integer). Returns a
-- zero matrix of the given size.
zeroQDiagHam :: Int -> Maybe QDiagHam
zeroQDiagHam n =
    if n > 0
    then Just $ QDiagHam n 0 empty
    else Nothing

-------------------------------------------------------------------------------
-- * Interface to diagonal matrix entries.

-- | Takes as input a diagonal index and a Hamiltonian. If the index is
-- in-bounds for the Hamiltonian, then the element at that index is returned.
-- Otherwise, nothing is returned.
getComp :: Int -> QDiagHam -> Maybe Rational 
getComp i h =
    if i >= 0 && i < dim h
    then Just $ IntMap.findWithDefault 0 i $ diag h
    else Nothing

-- | Takes as input a diagonal index, a rational number, and a Hamiltonian. If
-- the index is in-bounds for the Hamiltonian, then the element at that index
-- overwritten by the rational number and the trace is updated according. The
-- new Hamiltonian is returned. Otherwise, nothing is returned.
setComp :: Int -> Rational -> QDiagHam -> Maybe QDiagHam
setComp n new h =
    case getComp n h of
        Just old -> let tr   = trace h - old + new
                        elts = IntMap.insert n new $ diag h
                    in Just $ QDiagHam (dim h) tr elts
        Nothing -> Nothing 
