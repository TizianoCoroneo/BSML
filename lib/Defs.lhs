
\section{The most basic library}\label{sec:Basics}

This section describes a module which we will import later on.

\begin{code}
{-# LANGUAGE TemplateHaskell #-}
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

data KrM = KrM {_worlds :: Set World,
                _rel :: World -> Set World,
                _val :: Proposition -> Set World}
makeLenses ''KrM

type Team = Set World

support :: KrM -> Team -> Form -> Bool
support _ s Bot = null s
support _ s NE = not (null s)
-- support m s (Prop n) = all (\w -> w `elem` (m^.val)) s

\end{code}