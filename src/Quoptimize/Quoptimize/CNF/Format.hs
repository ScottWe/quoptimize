-- | Provides data formats for CNF encodings.

module Quoptimize.CNF.Format
  ( CNFSAT
  , Polarity(..)
  , addDisjunction
  , disjunctions
  , emptyCnfSat
  , literalCount
  , maxClauseLen
  , omitPolarity
  ) where

-------------------------------------------------------------------------------
-- * Constructors and Destructors for a Polarity Wrapper Type.

-- | Wraps a value with a positive or negative polarity.
data Polarity a = Positive a
                | Negative a
                deriving (Show, Eq)

-- | Returns the underlying value without a polarity.
omitPolarity :: Polarity a -> a
omitPolarity (Positive x) = x
omitPolarity (Negative x) = x

-------------------------------------------------------------------------------
-- * Constructors and Destructors for a CNF-SAT Encoding.

-- | A data format for CNFSAT instances.
--
-- Invariant: forall cnf of type CNFSAT:
--              forall disjunct in (disjunctions cnf):
--                forall atom in disjunct:
--                  0 <= (omitPolarity atom) < (literalCount cnf)
data CNFSAT = CNFSAT { literalCount :: Int
                     , maxClauseLen :: Int
                     , disjunctions :: [[Polarity Int]] 
                     } deriving (Show, Eq)

-- | Takes as input the number of literals in a CNF-SAT encoding. If the number
-- of literals is greater than zero, then returns a CNF-SAT problem with the
-- provided number of literals, and zero disjunctions. Otherwise, nothing is
-- returned.
emptyCnfSat :: Int -> Maybe CNFSAT
emptyCnfSat litCt =
    if litCt > 0
    then Just $ CNFSAT litCt 0 []
    else Nothing

-------------------------------------------------------------------------------
-- * Interfaces to Update a CNF-SAT Problem While Maintaining Type Invariants.

-- | Helper method to check if all atoms in a CNF-clause are within bounds.
-- Takes as input the number of literals and the list of atoms in a CNF-clause.
-- Assumes that the i-th literal maps to the integer i.
checkDisjunction :: Int -> [Polarity Int] -> Bool
checkDisjunction litCt = all f
    where f atom = let lit = omitPolarity atom
                   in lit >= 0 && lit < litCt

-- | Takes as input a disjunction and a CNF-SAT problem. If every literal used
-- within the disjunction is in-bounds for the CNF-SAT problem, then a new
-- CNF-SAT problem is returned by appending the new disjunction to the list of
-- disjunctions in the old CNF-SAT problem. Otherwise, nothing is returned.
addDisjunction :: [Polarity Int] -> CNFSAT -> Maybe CNFSAT
addDisjunction []       cnf                      = Just cnf
addDisjunction disjunct (CNFSAT litCt maxLen disjuncts) =
    if checkDisjunction litCt disjunct
    then Just $ CNFSAT litCt (max maxLen curLen) (disjunct:disjuncts)
    else Nothing
    where curLen = length disjunct
