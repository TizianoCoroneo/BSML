
\section{The most basic library}\label{sec:Basics}

This section describes a module which we will import later on.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Defs where

import Control.Monad

import Data.Set (Set, isSubsetOf, powerSet, unions, cartesianProduct)

import qualified Data.Set as Set
import Test.QuickCheck

type Proposition = Int

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

type World = Int

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

class AntiSupportable m s f where
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
  (m,s) |= Or f g  = any (\(t,u) -> Set.union t u == s && (m,t) |= f && (m, u) |= g) $ teamParts s
  (m,s) |= Dia f   = all (any (\t -> not (null t) && (m,t) |= f) . powerSet . rel m) s

instance AntiSupportable KrM Team Form where
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

-- instance Supportable KrM Team [Form] where
-- (m,s) |= fs = all
-- hello

\end{code}

Some example models.

\begin{code}

-- Aloni2024 - Figure 3c.
figure3 :: KrM
figure3 = KrM (Set.fromList [0, 1, 2, 3]) r v
  where r x = case x of
         0 -> Set.empty
         1 -> Set.empty
         2 -> Set.empty
         3 -> Set.fromList [1, 2]
         _ -> undefined
        v x = case x of
         1 -> Set.fromList [1, 3]
         2 -> Set.fromList [2, 3]
         _ -> undefined

figure3team :: Team
figure3team = Set.fromList [3]

figure3propC :: Bool
figure3propC = (figure3, figure3team) |= enrich (MDia (MProp 1 `MOr` MProp 2))

\end{code}