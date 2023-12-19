-- | Provides data formats for fmailies of Hamiltonian matrices.

module Quoptimize.Cost.CostPhase where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.List

import Quipper

import Quoptimize.Hamiltonian.Format

-------------------------------------------------------------------------------
-- ----------------------------------------------------------------------
-- * Hamiltonian data

-- | An index for an orbital. These are usually denotes p,q,r,s.
type Orbital = Integer

data Seat a =
  Noop a
  | Singleton a
  deriving (Show, Eq, Ord)


-- | A /stage/ is a list of seats. This amounts to a permutation of
-- [0,...,m-1], together with an indication of which pairs, triples,
-- and quads should be "active" in the stage.
type Stage a = [Seat a]


-- ----------------------------------------------------------------------
-- * Gates

-- | Apply a /T/(θ) gate. This is the gate
--
-- > \[1, 0]
-- > \[0, [exp -iθ]].
-- 
-- It is equivalent to [exp iθ/2Z] = 'expZt' (-iθ/2) up to a
-- phase. Note that /T/(-π/4) is the usual /T/-gate.
gse_T_at :: Double -> Qubit -> Circ ()
gse_T_at theta q = named_rotation_at "T(%)" theta q

-------------------------------------------------------------------------------
-- * The circuit for a Trotter step. (From New-GSE)
  
-- | Perform a single controlled Trotter timestep of the Hamiltonian
-- simulation.
--
-- The parameters are:
--
-- • /revert/: whether to perform the stages in the opposite order.
-- 
-- • /t/: the size of the elementary timestep
--
-- • /h/: the Hamiltonian data
--
-- • /bs/: a list of /b/ qubits, encoding an integer /k/ in binary, in
--   big-headian bit order, i.e., the head of the list contains the
--   most significant digit.
--
-- • /qs/: a list of /m/ qubits [q₀, ..., qₘ₋₁], corresponding to
--   orbitals.
--
-- It generates a circuit that computes the operator $e^{-itkH}$ on
-- the qubits /qs/, controlled by the integer /k/ encoded by the /bs/.
trotter_step :: Bool -> Timestep -> QDiagHam -> [Qubit] -> [Qubit] -> Circ ()
trotter_step revert t hamil bs qs = do
  do_stage t hamil bs qs
  return ()

  
-- | Generates a parallel rotation circuit correponding to a
-- stage, assuming the qubits are already in the indicated order.
do_stage :: Timestep -> QDiagHam -> [Qubit] -> [Qubit] -> Circ ()
do_stage t hamil bs qs = do 
  do_singletons t bs hs qs
  return ()
  where
    m = fromIntegral (length qs)
    hamil_index p = case getComp p hamil of 
      Just h -> fromRational h
      Nothing -> error "hamil_index"
    hs = [hamil_index p| p <- [0..m-1]]
    
    


-- ----------------------------------------------------------------------
-- * Singletons

-- | Apply a singleton circuit. /t/ is the timestep, /h00/ is the
-- Hamiltonian data, /bs/ are the precision bits in big-headian bit
-- order, e.g., [b2,b1,b0], and /o0/ is the orbital qubit.
{-do_singleton :: Timestep -> Double -> [Qubit] -> Qubit -> Circ ([Qubit])
do_singleton t h00 bs o0 = do
  with_computed wc $ \as -> do
    let angles = [(2^i * t * h00, x) | (i,x) <- as]
    qs <- sequence [gse_T_at θ x | (θ,x) <- angles]
    return ()
  where
    ibs = zip [0..] (reverse bs)
    wc = do
     as <- sequence [initialize_ancilla i [Signed b True, Signed o0 True] | (i, b) <- ibs] 
     return as
-}

-- | Plural of 'do_singleton'.
do_singletons :: Timestep -> [Qubit] -> [Double] -> [Qubit] -> Circ ()
do_singletons t bs angs qs = do
  with_computed (wcs angs qs) $ \ass -> do
    let angs_qs_ass = zip (zip angs qs) ass
    sequence_ [do_angles h00 o0 as | ((h00,o0),as) <- angs_qs_ass]
  where
    ibs = zip [0..] (reverse bs)
    wc o0 = do
     as <- sequence [initialize_ancilla i [Signed b True, Signed o0 True] | (i, b) <- ibs] 
     return as
    wcs angs qs = do
      sequence [wc o0 | (_,o0) <- zip angs qs]
    do_angles h00 o0 as = do
      let angles = [(2^i * t * h00, x) | (i,x) <- as]
      sequence_ [gse_T_at θ x | (θ,x) <- angles]
      return ()

-- ----------------------------------------------------------------------
-- | Create a new ancilla and initialize it from a list of qubits with
-- positive (\"True\") and/or negative (̈\"False\") controls.
initialize_ancilla :: Integer -> [Signed Qubit] -> Circ (Integer, Qubit)
initialize_ancilla i ctrls = do
  q <- qinit False
  qnot_at q `controlled` ctrls
  return (i, q)
