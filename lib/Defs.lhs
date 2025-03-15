
\section{Basic Definitions}\label{sec:Defs}

This section describes the basic definitions for the explicit model checker.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Defs where

import Control.Monad

import Data.Set (Set, isSubsetOf, powerSet, unions, cartesianProduct)
import qualified Data.Set as Set
import Test.QuickCheck

type Proposition = Int
type World = Int

-- Basic Modal Logic formulas
data MForm
  = MProp Proposition
  | MNeg MForm
  | MAnd MForm MForm
  | MOr  MForm MForm
  | MDia MForm
  deriving (Eq,Show)

-- BSML formulas
data Form
  = Bot
  | NE
  | Prop Proposition
  | Neg Form
  | And Form Form
  | Or  Form Form
  | Dia Form
  deriving (Eq,Show)


data KrM = KrM {
  worlds :: Set World,
  rel :: Rel,
  val :: Val}

type Rel = World -> Set World
type Val = Proposition -> Set World
type Team = Set World

teamRel :: KrM -> Team -> Set World
teamRel m s = unions $ Set.map (rel m) s

class Supportable m s f where
  support :: m -> s -> f -> Bool
  support = curry (|=)

  (|=) :: (m, s) -> f -> Bool
  (|=) = uncurry support

class Antisupportable m s f where
  antisupport :: m -> s -> f -> Bool
  antisupport = curry (=|)

  (=|) :: (m, s) -> f -> Bool
  (=|) = uncurry antisupport

teamParts :: Team -> Set (Team, Team)
teamParts = join cartesianProduct . powerSet

instance Supportable KrM World MForm where
  (m,w) |= MProp n  = w `elem` val m n
  (m,w) |= MNeg f   = not $ (m,w) |= f
  (m,w) |= MAnd f g = (m,w) |= f && (m,w) |= g
  (m,w) |= MOr f g  = (m,w) |= f || (m,w) |= g
  (m,w) |= MDia f   = any (\w' -> (m,w') |= f) (rel m w)

instance Supportable KrM Team Form where
  (_,s) |= Bot     = null s
  (_,s) |= NE      = not (null s)
  (m,s) |= Prop n  = s `isSubsetOf` val m n
  (m,s) |= Neg f   = (m,s) =| f
  (m,s) |= And f g = (m,s) |= f && (m,s) |= g
  (m,s) |= Or f g  = any (\(t,u) -> Set.union t u == s && (m,t) |= f && (m,u) |= g) $ teamParts s
  (m,s) |= Dia f   = all (any (\t -> not (null t) && (m,t) |= f) . powerSet . rel m) s

instance Antisupportable KrM Team Form where

  _     =| Bot     = True
  (_,s) =| NE      = null s
  (m,s) =| Prop n  = Set.disjoint s (val m n)
  (m,s) =| Neg f   = (m,s) |= f
  (m,s) =| And f g = any (\(t,u) -> Set.union t u == s && (m,t) =| f && (m,u) =| g) $ teamParts s
  (m,s) =| Or f g  = (m,s) =| f && (m,s) =| g
  (m,s) =| Dia f   = all (\w -> (m, rel m w) =| f) s


-- In Aloni2024 it is indicated as []+
enrich :: MForm -> Form
enrich (MProp n) = Prop n
enrich (MNeg n) = Neg (enrich n) `And` NE
enrich (MAnd p q) = enrich p `And` enrich q `And` NE
enrich (MOr p q) = (enrich p `Or` enrich q) `And` NE
enrich (MDia n) = Dia (enrich n) `And` NE

instance Supportable KrM Team [Form] where
  support = (all .) . support

instance Antisupportable KrM Team [Form] where
  antisupport = (all .) . antisupport

box :: Form -> Form
box = Neg . Dia . Neg

botbot :: Form
botbot = And Bot NE

top :: Form
top = NE

toptop :: Form
toptop = Neg Bot

bigor :: [Form] -> Form
bigor [] = Bot
bigor fs = foldr1 Or fs

bigand :: [Form] -> Form
bigand [] = toptop
bigand fs = foldr1 And fs

subsetOf :: Ord a => Set a -> Gen (Set a)
subsetOf s = Set.fromList <$> sublistOf (Set.toList s)

genFunctionToSubset :: Ord a => CoArbitrary a => Set a -> Gen (Int -> Set a)
genFunctionToSubset ws = do
  outputs <- vectorOf (length ws) (subsetOf ws)
  fmap (\f x -> f x ! outputs) arbitrary

(!) :: Int -> [Set a] -> Set a
(!) _ [] = Set.empty
(!) i xs = xs !! (i `mod` length xs)

instance Arbitrary KrM where
  arbitrary = sized (\s -> do
    ws <- Set.fromList <$> vectorOf s arbitrary
    r <- genFunctionToSubset ws
    v <- genFunctionToSubset ws
    return (KrM ws r v))

instance Show KrM where
  show (KrM ws _ _) = "KrM (" ++ show ws ++ ") (*) (*)" -- TODO: improve

\end{code}

Some example models.

\begin{code}
-- Aloni2024 - Figure 3.
w0, wp, wq, wpq :: Int
wp  = 0
wq  = 1
wpq = 2
w0  = 3

u3 :: Set World
u3 = Set.fromList [0..3]

r3a, r3b, r3c :: World -> Set World
r3a = const Set.empty

r3b wpq = Set.fromList [wp, wpq]
r3b w0  = Set.singleton wq
r3b _   = Set.empty

r3c wpq = Set.fromList [wp, wq]
r3c _   = Set.empty

v3 :: Proposition -> Set World
v3 1 = Set.fromList [wp, wpq]
v3 2 = Set.fromList [wq, wpq]
v3 _ = Set.empty

m3a, m3b, m3c :: KrM
m3a = KrM u3 r3a v3
m3b = KrM u3 r3b v3
m3c = KrM u3 r3c v3

s3a1, s3a2, s3b, s3c :: Team
s3a1 = Set.singleton wq
s3a2 = Set.fromList [wp, wq]
s3b = Set.fromList [wpq, w0]
s3c = Set.singleton wpq

figure3propC :: Bool
figure3propC = (m3c, s3c) |= enrich (MDia (MProp 1 `MOr` MProp 2))

\end{code}
