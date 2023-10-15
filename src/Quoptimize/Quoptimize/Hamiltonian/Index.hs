-- | Utility to convert SAT solutions to Hamiltonian indices.

module Quoptimize.Hamiltonian.Index
  ( BinaryIndex
  , addVar
  , evaluateIndex
  , getBinaryLength 
  , nullBinaryIndex
  ) where

-------------------------------------------------------------------------------
-- * Conversion from SAT Solutions to Indices.

-- | An interface to view SAT solutions as (2^n)-by-(2^n) matrix indices.
-- Recall that a SAT solution is of the form (x1)(x2)(x3)...(xn) when expressed
-- as a bianry string.
data BinaryIndex = BinaryIndex Int Int

-- | Returns the index corresponding to the vacuous solution to a SAT-instance
-- with zero variables.
nullBinaryIndex :: BinaryIndex
nullBinaryIndex = BinaryIndex 0 0

-- | Interprets a SAT-solution as a matrix index.
evaluateIndex :: BinaryIndex -> Int
evaluateIndex (BinaryIndex idx _) = idx

-- | Returns the number of variables in the SAT-instance.
getBinaryLength :: BinaryIndex -> Int
getBinaryLength (BinaryIndex _ len) = len

-- | Introduces a new variable to the solution at the next available index.
addVar :: BinaryIndex -> Bool -> BinaryIndex
addVar (BinaryIndex idx len) bit = BinaryIndex idx' len'
    where len' = len + 1
          idx' = idx * 2 + fromEnum bit
