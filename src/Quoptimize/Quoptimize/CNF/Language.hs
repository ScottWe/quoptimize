-- | Haskell representation for a variant of the DIMACS language.

module Quoptimize.CNF.Language
  ( DimacsAtom(..)
  , DimacsFile(..)
  , NegInt
  , PosInt
  , fromNegInt
  , fromPosInt
  , toNegInt
  , toPosInt
  ) where

-------------------------------------------------------------------------------
-- * Positive Integer "Dependant" Type.

-- | Type for positive integers.
newtype PosInt = PosInt Int deriving (Show, Eq)

-- | Type-safe conversion from Int to PosInt. A type violation returns nothing.
toPosInt :: Int -> Maybe PosInt
toPosInt n = if n > 0 then Just $ PosInt n else Nothing

-- | Casts a PosInt to an Int.
fromPosInt :: PosInt -> Int
fromPosInt (PosInt n) = n

-------------------------------------------------------------------------------
-- * Negative Integer "Dependant" Type.

-- | Type for negatives integers.
newtype NegInt = NegInt Int deriving (Show, Eq)

-- | Type-safe conversion from Int to NegInt. A type violation returns nothing.
toNegInt :: Int -> Maybe NegInt
toNegInt n = if n < 0 then Just $ NegInt n else Nothing

-- | Casts a NegInt to an Int.
fromNegInt :: NegInt -> Int
fromNegInt (NegInt n) = n

-------------------------------------------------------------------------------
-- * DIMACS Format Types.

-- | Represents a DIMACS atom. This is either a strictly positive integer, or a
-- strictly negative integer.
data DimacsAtom = PosLit PosInt
                | NegLit NegInt
                deriving (Show, Eq)

-- | Represents a valid DIMACS file (from the perspective of types). This does
-- not guarentee that the file has the correct number or clauses, nor does it
-- guarentee that all literals are defined.
data DimacsFile = DimacsCNF PosInt PosInt [[DimacsAtom]] deriving (Show, Eq)
