module ND where

import Syntax

import Data.Set (Set)
import qualified Data.Set as Set

hasNE :: Form -> Bool
hasNE = hasCr _NE

hasGor :: Form -> Bool
hasGor = hasCr _Gor

data Proof = Prf {conclusion :: Form,
                  assumptions :: Set Form}

sorry :: Form -> Proof
sorry = flip Prf Set.empty
{-# WARNING sorry "Proof uses sorry!" #-}

assume :: Form -> Proof
assume = Prf <*> Set.singleton

-- (a) Rules for &

andIntro :: Proof -> Proof -> Proof
andIntro (Prf f fAss) (Prf g gAss) = Prf (And f g) $ fAss <> gAss

andElimL :: Proof -> Proof
andElimL (Prf (And f _) ass) = Prf f ass
andElimL _ = error "Cannot apply &Elim, conclusion is not a conjunction!"

andElimR :: Proof -> Proof
andElimR (Prf (And _ g) ass) = Prf g ass
andElimR _ = error "Cannot apply &Elim, conclusion is not a conjunction!"

-- (b) Rules for ~

negIntro :: Form -> Proof -> Proof
negIntro f (Prf g ass)
  | g /= Bot        = error "Cannot apply ~Intro, conclusion is not _|_!"
  | not $ isBasic f = error "Cannot apply ~Intro, conclusion is not basic!"
  | any hasNE ass   = error "Cannot apply ~Intro, there are undischarged assumptions containing NE!"
  | otherwise       = Prf (Neg f) $ Set.delete f ass

negElim :: Form -> Proof -> Proof -> Proof
negElim g (Prf f ass) (Prf f' ass')
  | (not . all isBasic) [f, f', g] = error "Cannot apply ~Elim non-basic formula!"
  | f /= Neg f' && Neg f /= f'     = error "Cannot apply ~Elim, conclusions are not contradictory!"
  | otherwise                      = Prf g $ ass <> ass'

negnegElim :: Proof -> Proof
negnegElim (Prf (Neg (Neg f)) ass) = Prf f ass
negnegElim _ = error "Cannot apply ~~Elim, conclusion is not a double negation!"

dmAnd :: Proof -> Proof
dmAnd (Prf (Neg (And f g)) ass) = Prf (Neg f `Or` Neg g) ass
dmAnd _ = error "Cannot apply &-De Morgan's law, conclusion is not negated conjunction!"

dmOr :: Proof -> Proof
dmOr (Prf (Neg (Or f g)) ass) = Prf (Neg f `And` Neg g) ass
dmOr _ = error "Cannot apply v-De Morgan's law, conclusion is not negated disjunction!"

neNegElim :: Proof -> Proof
neNegElim (Prf (Neg NE) ass) = Prf Bot ass
neNegElim _ = error "Cannot apply ~NE-Elim, conclusion is not ~NE!"

-- (c) Rules for v

orIntro :: Form -> Proof -> Proof
orIntro g (Prf f ass)
  | hasNE g   = error "Cannot vIntro a formula containing NE!"
  | otherwise = Prf (Or f g) ass

orWkn :: Proof -> Proof
orWkn (Prf f ass) = Prf (Or f f) ass

orComm :: Proof -> Proof
orComm (Prf (Or f g) ass) = Prf (Or g f) ass
orComm _ = error "Cannot apply vCom, conclusion is not a disjunction!"

orAss :: Proof -> Proof
orAss (Prf (f `Or` (g `Or` h)) ass) = Prf ((f `Or` g) `Or` h) ass
orAss _ = error "Cannot apply vAss, conclusion is not a nested disjunction!"

orElim :: Proof -> Proof -> Proof -> Proof
orElim (Prf (Or f g) ass) (Prf h ass1) (Prf h' ass2)
  | h /= h' = error "Cannot apply vElim, conclusions of latter proofs do not match!"
  | any hasNE ass' = error "Cannot apply vElim, latter proofs have undischarged non-basic assumptions."
  | otherwise = Prf h $ ass <> ass'
  where ass' = Set.delete f ass1 <> Set.delete g ass2
orElim _ _ _ = error "Cannot apply vElim, conclusion of first proof is not a disjunction!"

orMon :: Proof -> Proof -> Proof
orMon (Prf (Or f g) ass) (Prf h ass1)
  | (not . all isBasic) ass' = error "Cannot apply vMon, latter proof has undischarged non-basic assumptions."
  | otherwise = Prf (Or f h) $ ass <> ass'
  where ass' = Set.delete g ass1
orMon _ _ = error "Cannot apply vMon"

-- (d) Rules for _|_ and NE

botElim :: Proof -> Proof
botElim (Prf (Or Bot f) ass) = Prf f ass
botElim _ = error "Cannot apply _|_Elim, conclusion is not of the form _|_ v phi!"

botbotCtr :: Form -> Proof -> Proof
botbotCtr g (Prf (Or (Bot `And` NE) _) ass) = Prf g ass
botbotCtr _ _ = error "Cannot apply _||_Ctr, conclusion is not of the form _||_ v phi!"

-- (e) Basic modal rules

diaMon :: Proof -> Proof -> Proof
diaMon (Prf g ass) (Prf (Dia f) ass1)
  | ass `Set.isSubsetOf` Set.singleton f = Prf (Dia g) ass1
  | otherwise = error "Cannot apply <>Mon, former proof has undischarged assumptions!"
diaMon _ _ = error "Cannot apply <>Mon, conclusion of latter proof is not of the form <>phi!"

boxMon :: Proof -> [Proof] -> Proof
boxMon (Prf g ass) ps
  | Set.map box ass `Set.isSubsetOf` Set.fromList (map conclusion ps) =
    Prf (box g) $ foldMap assumptions ps
  | otherwise = error "Cannot apply []Mon, former proof has undischarged assumptions!"

diaBoxInter :: Proof -> Proof
diaBoxInter = undefined

-- (f) Rules governing the interaction of the modalities and connectives

diaSep :: Proof -> Proof
diaSep = undefined

diaJoin :: Proof -> Proof -> Proof
diaJoin = undefined

boxInst :: Proof -> Proof
boxInst = undefined

boxDiaJoin :: Proof -> Proof -> Proof
boxDiaJoin = undefined

-- (g) Propositional rules involving \V

gorIntroL :: Form -> Proof -> Proof
gorIntroL = undefined

gorIntroR :: Form -> Proof -> Proof
gorIntroR = undefined

gorElim :: Proof -> Proof -> Proof -> Proof
gorElim = undefined

orGorDistr :: Proof -> Proof
orGorDistr = undefined

dmGor :: Proof -> Proof
dmGor = undefined

neIntro :: Proof
neIntro = undefined

-- (h) Modal rules for \V

diaGorOrConv :: Proof -> Proof
diaGorOrConv = undefined

boxGorOrConv :: Proof -> Proof
boxGorOrConv = undefined