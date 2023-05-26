-- | Provides data formats for CNF encodings.

module Quoptimize.CNF.Format
  ( CNFSAT
  , DimacsError(..)
  , Polarity(..)
  , addDisjunction
  , disjunctions
  , emptyCnfSat
  , fromDimacsFile
  , literalCount
  , maxClauseLen
  , omitPolarity
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Quoptimize.CNF.Language
  ( DimacsAtom(..)
  , DimacsFile(..)
  , fromNegInt
  , fromPosInt
  , toPosInt
  )

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
addDisjunction []       cnf                             = Just cnf
addDisjunction disjunct (CNFSAT litCt maxLen disjuncts) =
    if checkDisjunction litCt disjunct
    then Just $ CNFSAT litCt (max maxLen curLen) (disjunct:disjuncts)
    else Nothing
    where curLen = length disjunct

-------------------------------------------------------------------------------
-- * Converting Raw DIMACS to CNFSAT.

-- | Indicates why a DIMACS file could not be interpreted as a CNFSAT problem.
data DimacsError = WrongClauseCount Int Int
                 | UndeclaredLitInClause Int
                 | UnexpectedSizeError
                 deriving (Eq, Show)

-- | Helper method to convert DimacsAtoms to CNFSAT polarized literals. There
-- are two major differences. First is that DimacsAtoms are 1-indexed, whereas
-- polarized literals are 0-indexed. Second is that DimacsAtoms are signed
-- whereas the underlying integer of a polarized literal is non-negative.
fromDimacsAtom :: DimacsAtom -> Polarity Int
fromDimacsAtom (NegLit dlit) = Negative lit
    where lit = negate $ fromNegInt dlit + 1
fromDimacsAtom (PosLit dlit) = Positive lit
    where lit = fromPosInt dlit - 1

-- | Helper method to add a list of DimacAtom clauses to a CNFSAT encoding with
-- the help of fromDimacsAtom. If a clause is rejected, then the index of the
-- clause is returned.
addDimacsDisjuncts :: Int -> [[DimacsAtom]] -> CNFSAT -> Either Int CNFSAT
addDimacsDisjuncts _ []                   cnf = Right cnf
addDimacsDisjuncts n (disjunct:disjuncts) cnf =
    case addDisjunction (map fromDimacsAtom disjunct) cnf of
        Just cnf' -> addDimacsDisjuncts (n + 1) disjuncts cnf'
        Nothing   -> Left n

-- | Helper method to determine the index of a DIMACS atom
getDimacsIndex :: DimacsAtom -> Int
getDimacsIndex (NegLit dlit) = -(fromNegInt dlit)
getDimacsIndex (PosLit dlit) = fromPosInt dlit

-- | Helper method to determine the maximum literal index in use across all
-- disjuctions in a CNF encoding.
getMaxLitInDisjuncts :: [[DimacsAtom]] -> Int
getMaxLitInDisjuncts = foldr f 0
    where g atom     n = max n $ getDimacsIndex atom
          f disjunct n = max n $ foldr g 0 disjunct

-- | Takes as input a DimacsFile, and returns as output an equivalent CNFSAT
-- problem. If conversion is not possible, then an erorr is returned instead.
fromDimacsFile :: DimacsFile -> Either DimacsError CNFSAT
fromDimacsFile (DimacsCNF litCt disjunctCt disjuncts) =
    if expect /= actual
    then Left $ WrongClauseCount expect actual
    else case emptyCnfSat $ fromPosInt litCt of
        Just cnf -> case addDimacsDisjuncts 1 disjuncts cnf of
            Left n     -> Left $ UndeclaredLitInClause n
            Right cnf' -> Right cnf' 
        Nothing -> Left UnexpectedSizeError
    where expect = fromPosInt disjunctCt
          actual = length disjuncts
fromDimacsFile (DimacsCNFMin disjuncts) =
    case toPosInt $ getMaxLitInDisjuncts disjuncts of
        Just litCt -> case emptyCnfSat $ fromPosInt litCt of
            Just cnf -> case addDimacsDisjuncts 1 disjuncts cnf of
                Left n     -> Left $ UndeclaredLitInClause n
                Right cnf' -> Right cnf' 
            Nothing -> Left UnexpectedSizeError
        Nothing -> Left UnexpectedSizeError
