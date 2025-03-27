\subsection{Semantics}
\label{sec:BSML_semantics}

We quickly recall the most important aspects of the semantics of BSML from \cite{Aloni2024}.
We interpret formulas on (Kripke) models, which consist of
\begin{itemize}
  \item a set of worlds $W$;
  \item a binary relation $R \subseteq W \times W$ between worlds;
  \item and a valuation $V : \texttt{Prop} \to \wp W$ mapping
propositional letters to the worlds in which they are satisfied.
\end{itemize}
A \emph{team} or \emph{state} (we will use these terms interchangably) on a model is a subset $s \subseteq W$.
To link back to the introduction, the worlds represent information-configurations and a
team represents the worlds that a speaker perceives as possible.

As the name \emph{Bilateral State-based Modal Logic} suggests, formulas are
evaluated with respect to a team (rather than a world).
The ``bilateral'' part refers to the fact that we make use of \emph{two} fundamental
semantics notions; \emph{support} and \emph{anti-support}, rather than just \emph{truth}.
Support (resp. antisupport) of a formula by a team represents the speaker's ability to
assert (resp. reject) the formula, given the worlds deemed possible in the team.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Semantics where

import Syntax

import Data.Set (Set)
import Data.IntMap (IntMap)
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import Data.List
\end{code}

In our implementation, we will represent worlds as \verb|Int|'s and teams as
lists of worlds.
For the relation and valuation, we use \verb|IntMap|'s, which are conceptually
identical to association lists with \verb|Int|-valued keys, but allow for fast lookup
of the successors and satisfied proposition letters of a world.

\begin{code}
type World = Int
type Team = [World]
type Rel = IntMap Team
type Val = IntMap (Set Proposition)

data KrM = KrM {worlds :: Team,
                rel    :: Rel,
                val    :: Val}
  deriving (Show)
\end{code}

We also define the following shorthands for looking up successors and valuations of worlds,
that allow us to treat \verb|rel| and \verb|val| as (partial) functions:

\begin{code}
rel' :: KrM -> World -> Team
rel' = (IntMap.!) . rel

val' :: KrM -> World -> Set Proposition
val' = (IntMap.!) . val
\end{code}

To allow defining a (anti)support-relation for different structures as well, we use the
following typeclasses, that contain a both a curried and uncurried version of
the function for (anti)support for ease of use
(only one is required to be provided; the other is the curried/uncurried equivalent).

\begin{code}
class Supportable model state formula where
  support :: model -> state -> formula -> Bool
  support = curry (|=)

  (|=) :: (model, state) -> formula -> Bool
  (|=) = uncurry support

  {-# MINIMAL (|=) | support #-}

class Antisupportable model state formula where
  antisupport :: model -> state -> formula -> Bool
  antisupport = curry (=|)

  (=|) :: (model, state) -> formula -> Bool
  (=|) = uncurry antisupport

  {-# MINIMAL (=|) | antisupport #-}
\end{code}

Before we implement the semantics, we need a function that computes all pairs of
teams whose union is a given team $s$.
Naively, we may define e.g.
\begin{showCode}
teamParts :: Team -> [(Team, Team)]
teamParts s = [(t,u) | t <- subsequences s, u <- subsequences s, sort . nub (t ++ u) == sort s]
\end{showCode}

Computationally however, this is incredibly expensive and will form a major
bottleneck for the efficiency of the model checking.
While finding such partitions is inherently exponential in complexity, we can
still do slightly better than the above:

\begin{code}
teamParts :: Team -> [(Team, Team)]
teamParts s = do
    t <- subsequences s
    u <- subsequences t
    return (t, s \\ u)
\end{code}

\begin{code}
instance Supportable KrM Team Form where
  (_,s) |= Bot     = null s
  (_,s) |= NE      = not (null s)
  (m,s) |= Prop n  = all (elem n . val' m) s
  (m,s) |= Neg f   = (m,s) =| f
  (m,s) |= And f g = (m,s) |= f && (m,s) |= g
  (m,s) |= Or f g  = any (\(t,u) -> (m,t) |= f && (m,u) |= g) $ teamParts s
  (m,s) |= Dia f   = all (any (\t -> not (null t) && (m,t) |= f) . subsequences . rel' m) s

instance Antisupportable KrM Team Form where
  _     =| Bot     = True
  (_,s) =| NE      = null s
  (m,s) =| Prop n  = not $ any (elem n . val' m) s
  (m,s) =| Neg f   = (m,s) |= f
  (m,s) =| And f g = any (\(t,u) -> (m,t) =| f && (m,u) =| g) $ teamParts s
  (m,s) =| Or f g  = (m,s) =| f && (m,s) =| g
  (m,s) =| Dia f   = all (\w -> (m, rel' m w) =| f) s
\end{code}