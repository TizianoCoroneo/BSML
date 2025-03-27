{-# LANGUAGE InstanceSigs #-}
\subsection{Semantics}
\label{sec:BSML_semantics}

We quickly recall the most important aspects of the semantics of BSML from \cite{Aloni2024}.
We interpret formulas on (Kripke) models, which consist of
\begin{itemize}
  \item a set of worlds $W$;
  \item a binary relation $R \subseteq W \times W$ between worlds;
  \item and a valuation $V : W \to \wp(\texttt{Prop})$ mapping a world to the propositions that hold in it.
  \footnote{In the paper, the valuation is defined as a function $\texttt{Prop} -> \wp W$, but this is easily seen to be equivalent.}
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

import Data.List
import Data.Set (Set)
import Data.IntMap (IntMap)
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap

import Test.QuickCheck
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
teamParts s = [(t,u) | t <- ps, u <- ps, sort . nub (t ++ u) == sort s]
  where ps = subsequences s
\end{showCode}

Computationally however, this is incredibly expensive and will form a major
bottleneck for the efficiency of the model checking.
While finding such partitions is inherently exponential in complexity, we can
still do slightly better (at least on average) than the above:

\begin{code}
teamParts :: Team -> [(Team, Team)]
teamParts s = do
    t <- subsequences s
    u <- subsequences t
    return (t, s \\ u)
\end{code}

Now, we are ready to define our semantics, in accordance to \cite{Aloni2024}:

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

One may also easily extend the above semantics to lists of formulae, as shown below.

\begin{code}
instance Supportable KrM Team [Form] where
  support = (all .) . support

instance Antisupportable KrM Team [Form] where
  antisupport = (all .) . antisupport
\end{code}

\subsection{Random models}
As for formulas, we want to be able to generate random models to verify properties
of BSML, and will use QuickCheck for this.

We will need a function that generates a \verb|Set| containing random elements of a list,
so we define the following analogue of \verb|sublistOf|:

\begin{code}
subsetOf :: Ord a => [a] -> Gen (Set a)
subsetOf = (Set.fromList <$>) . sublistOf
\end{code}

Then, an arbitrary model $M = (W, R, V)$ can be generated as follows:

\begin{code}
instance Arbitrary KrM where
  arbitrary = sized $ \n -> do
\end{code}
The size parameter $n$ gives an upper bound to the amount of worlds; we pick some
$k \leq n$ and define $W = {0, 1, \dots, k}$.
For every $w <= k$, we then generate a random set of successors $R[w]$ and random set
of propositions $V(w)$ that hold at $w$.
\begin{code}
    k <- choose (0, n)
    let ws = [0..k]
    r <- IntMap.fromList . zip ws <$> vectorOf (k+1) (sublistOf [0..k])
    v <- IntMap.fromList . zip ws <$> vectorOf (k+1) (subsetOf [1..numProps])
    return (KrM ws r v)
\end{code}
When finding counterexamples, it is useful to find models that are as small as possible,
so we also define \verb|shrink| that tries to restrict the worlds of the model.
\begin{code}
  shrink m = do
    ws' <- subsequences $ worlds m
    let r' = IntMap.fromList [(w, rel' m w `intersect` ws') | w <- ws']
    let v' = IntMap.filterWithKey (const . (`elem` ws')) $ val m
    return (KrM ws' r' v')
\end{code}

When testing, we will often want to generate a random model \emph{with} a random
team or world of that model.
Generating a random \verb|Int| or \verb|[Int]| would not work then, since there
is no guarantee that the generated value is a valid team/world in the model.
To remedy that, we define wrappers for models with a team/world and define
\verb|Arbitrary|-instances for those wrappers:

\begin{code}
data TeamPointedModel = TPM KrM Team
  deriving (Show)

data WorldPointedModel = WPM KrM World
  deriving (Show)

instance Arbitrary TeamPointedModel where
  arbitrary = do
    m <- arbitrary
    s <- sublistOf $ worlds m
    return (TPM m s)

  shrink (TPM m s) = filter (\(TPM m' s') -> s' `isSubsequenceOf` worlds m') (flip TPM s <$> shrink m)

instance Arbitrary WorldPointedModel where
  arbitrary = do
    m <- arbitrary
    w <- elements $ worlds m
    return (WPM m w)

  shrink (WPM m w) = filter (\(WPM m' w') -> w' `elem` worlds m') (flip WPM w <$> shrink m)
\end{code}
Note that when shrinking, we should only allow shrinks where the world/team is still
contained in the model.