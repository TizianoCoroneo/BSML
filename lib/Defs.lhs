
\section{The most basic library}\label{sec:Basics}

This section describes a module which we will import later on.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Defs where

import Data.Set (Set, isSubsetOf, (\\), powerSet, union, unions, map, disjoint)
import qualified Data.Set as Set
import Test.QuickCheck (disjoin)

type Proposition = Int

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

data KrM = KrM {worlds :: Set World,
                rel :: World -> Set World,
                val :: Proposition -> Set World}

type Team = Set World

teamRel :: KrM -> Team -> Set World
teamRel m s = unions $ Set.map (rel m) s

support :: KrM -> Team -> Form -> Bool
support _ s Bot = null s
support _ s NE = not (null s)
support m s (Prop n) = s `isSubsetOf` val m n
support m s (Neg f) = antisupport m s f
support m s (And f g) = support m s f && support m s g
support m s (Or f g) = any (\t -> support m t f && support m (s \\ t) g) (powerSet s)
support m s (Dia f) = all (any (\t -> not (null t) && support m t f) . powerSet . rel m) s

antisupport :: KrM -> Team -> Form -> Bool
antisupport _ _ Bot = True
antisupport _ s NE = null s
antisupport m s (Prop n) = disjoint s (val m n)
antisupport m s (Neg f) = support m s f
antisupport m s (Or f g) = antisupport m s f && antisupport m s g
antisupport m s (And f g) = any (\t -> antisupport m t f && antisupport m (s \\ t) g) (powerSet s)
antisupport m s (Dia f) = all (\w -> antisupport m (rel m w) f) s

class Supportable m s f where
  (|=) :: (m, s) -> f -> Bool

class AntiSupportable m s f where
  (=|) :: (m, s) -> f -> Bool

instance Supportable KrM Team Form where
  (|=) = uncurry support

instance AntiSupportable KrM Team Form where
  (=|) = uncurry antisupport

-- instance Supportable KrM Team [Form] where
-- (m,s) |= fs = all
-- hello

\end{code}