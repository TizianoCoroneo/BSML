
\section{The most basic library}\label{sec:Basics}

This section describes a module which we will import later on.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Defs where

import Data.Set (Set)

import Control.Lens

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

support :: KrM -> Team -> Form -> Bool
support _ s Bot = null s
support _ s NE = not (null s)
support m s (Prop n) = all (`elem` val m n) s


class Supportable m s f where
  (|=) :: (m, s) -> f -> Bool

class AntiSupportable m s f where
  (=|) :: (m, s) -> f -> Bool

instance Supportable KrM Team Form where
  (|=) = uncurry support

instance AntiSupportable KrM Team Form where
  (=|) = uncurry antisupport

\end{code}