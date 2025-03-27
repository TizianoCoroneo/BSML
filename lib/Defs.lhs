
\section{Bilateral State-Based Modal Logic}\label{sec:BSML}



This section describes the basic definitions for the explicit model checker for Bilateral State-Based Modal Logic (henceforth BSML). We begin by importing modules necessary
for this. Unlike previous model checkers we have seen (which use lists), we utilise sets in our models. We do this to
prepare for the eventuality of using IntSets - which are a much more efficient structure for storing and retrieving integers than
lists.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Defs where

-- Potential TODO: Change Set to IntSet (and Map to IntMap) for performance.
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Test.QuickCheck
\end{code}

We establish the data type \texttt{Form}, which we use to describe BSML formulae. We later establish the data type
\texttt{MForm} to describe modal formulae in the basic modal language.

\begin{code}

type Proposition = Int
type World = Int

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

\end{code}

BSML relies on team semantics, and a team is a set of worlds. A model in this logic (much like one in modal logics like $\mathsf{K}$ or $\mathsf{S4}$)
consists of a set of worlds, a relation on the set of worlds and a valuation function; which tells us which propositions are true at a
given world. We store the relation as a function from the set of Worlds to the powerset of Worlds - it is effectively a successor function.
This gives us easy access to the successors of any given world, without needing to perform lookup operations for the same.

\begin{code}

type Team = Set World
type Rel = Map World (Set World)
type Val = Map World (Set Proposition)

data KrM = KrM {worlds :: Set World,
                rel    :: Rel,
                val    :: Val}
  deriving (Show)

data TeamPointedModel = TPM KrM Team
  deriving (Show)

data WorldPointedModel = WPM KrM World
  deriving (Show)
\end{code}

We define below \texttt{rel'} and \texttt{val'}; two functions that help us get the successors of a particular world in a model,
and the propositions true at a given world in the model respectively.

\begin{code}

rel' :: KrM -> World -> Set World
rel' = (Map.!) . rel

val' :: KrM -> World -> Set Proposition
val' = (Map.!) . val

\end{code}

Finally, we describe a function that gives us the successors of all worlds in a given team.

\begin{code}

teamRel :: KrM -> Team -> Set World
teamRel m s = Set.unions $ Set.map (rel m Map.!) s

\end{code}

We define now notions of supportability and antisupportability for formulae with respect to a model and a team. Supportability's closest
analogue in more familiar logics is $\vDash$, although the definition varies slightly since we now have a new-operator (\texttt{NE} or non-empty)
to contend with. Antisupportability is defined analogously to negation as will be evident below.

We also define classes \texttt{Supportable} and \texttt{Antisupportable}, and present two alternate definitions of the support (and dually, the antisupport)
function. The minimal definition for the class only requires one of these to be provided; the other is the curried/uncurried equivalent.

\begin{code}

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
\end{code}

We define now the semantics for BSML. For more detail, the reader may refer to page 5 of \cite{Aloni2024}, but the gist of it is
that most of the definitions would be familiar to any reader well-versed in modal logics such as $\mathsf{K}$. From below, it should
become clear why we mentioned earlier that \texttt{antisupport} acts like negation.

Defining the semantics of $\lor$ for \texttt{support} and $\land$ for \texttt{antisupport} required us to use a helper function - \texttt{teamParts}.
This function provides computes set of all pairs of subsets whose union is a given team $s$.

\begin{code}
teamParts :: Team -> Set (Team, Team)
teamParts s = Set.fromList $ do
    s' <- ps
    s'' <- Set.toList $ Set.powerSet s'
    let augmentedCompl = Set.difference s s''
    return (s' :: Team, augmentedCompl)
  where ps = Set.toList $ Set.powerSet s

instance Supportable KrM Team Form where
  (_,s) |= Bot     = null s
  (_,s) |= NE      = not (null s)
  (m,s) |= Prop n  = all (elem n) $ Set.map (val' m) s
  (m,s) |= Neg f   = (m,s) =| f
  (m,s) |= And f g = (m,s) |= f && (m,s) |= g
  (m,s) |= Or f g  = any (\(t,u) -> t <> u == s && (m,t) |= f && (m,u) |= g) $ teamParts s
  (m,s) |= Dia f   = all (any (\t -> not (null t) && (m,t) |= f) . Set.powerSet . rel' m) s

instance Antisupportable KrM Team Form where
  _     =| Bot     = True
  (_,s) =| NE      = null s
  (m,s) =| Prop n  = not . any (elem n) $ Set.map (val' m) s
  (m,s) =| Neg f   = (m,s) |= f
  (m,s) =| And f g = any (\(t,u) -> t <> u == s && (m,t) =| f && (m,u) =| g) $ teamParts s
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

We write first a function that describes $\Box$ formulae - much like in several standard modal logics, $\Box \varphi \equiv \neg \lozenge \neg \varphi$
for any formula $\varphi$.

We note that $\bot$ as defined here is only false on all non-empty teams. It is therefore referred to in \cite{Aloni2024} as the
\textit{weak contradiction}. The strong contradiction (referred to in \cite{Aloni2024} as $\botbot$) is the formula $\bot \land \texttt{NE}$, which
is called \texttt{botbot} below. Dually, the formula \texttt{NE} serves as the \textit{weak tautology} here; it is true
on all non-empty teams. The formula $\toptop \coloneqq \neg \bot$ is true everywhere, and is therefore called the \textit{strong tautology}.

\begin{code}

box :: Form -> Form
box = Neg . Dia . Neg

botbot :: Form
botbot = And Bot NE

top :: Form
top = NE

toptop :: Form
toptop = Neg Bot

\end{code}

We may use the above to interpret empty disjunctions and conjunctions as below:

\begin{code}

bigor :: [Form] -> Form
bigor [] = Bot
bigor fs = foldr1 Or fs

bigand :: [Form] -> Form
bigand [] = toptop
bigand fs = foldr1 And fs

\end{code}

\begin{code}

subsetOf :: Ord a => Set a -> Gen (Set a)
subsetOf s = Set.fromList <$> sublistOf (Set.toList s)

\end{code}

The following code block implements the Arbitrary typeclass for models (KrM), pointed models (TeamPointedModel or WorldPointedModel), and formulas (both BML formulas in MForm, and BSML formulas in Form).

We start by defining some parameters that will be used in the generators.

The proposition are picked in the range (1, maximumArbitraryMFormPropositions) for MForm, and from (1, maximumArbitraryMFormPropositions) for Form.
We cannot use the QuickCheck size because it would introduce a bias in the generation of Proposition values, where small sized examples can only choose small valued Propositions.

arbitraryFormScaling is used to manage the scaling of the arbitrary formulas, while arbitraryPropScaling is used to manage the scaling of the count of supported propositions in the valuation function of a model.

\begin{code}
maximumArbitraryMFormPropositions :: Int
maximumArbitraryMFormPropositions = 32

maximumArbitraryFormPropositions :: Int
maximumArbitraryFormPropositions = 10

arbitraryFormScaling :: Int
arbitraryFormScaling = 10

arbitraryPropScaling :: Int
arbitraryPropScaling = 5
\end{code}

The instance for a Kripke model KrM first generates an arbitrary set of worlds ws, from 0 to an arbitrary k; then, it generates an arbitrary Map world to subset of all worlds to represent the model relation. Finally, it generates the valuation map by picking an arbitrary list of propositions from the range (0, maximumArbitraryFormPropositions), where the count of item in the lists is scaled by arbitraryPropScaling down from the QuickCheck size parameter.

\begin{code}
instance Arbitrary KrM where
  arbitrary = sized (\n -> do
    k <- choose (0, n)
    let ws = Set.fromList [0..k]
    r <- Map.fromList . zip [0..k] <$> vectorOf (k+1) (subsetOf ws)
    v <- Map.fromList . zip [0..k] <$> vectorOf (k+1) (Set.fromList <$> scale (`div` arbitraryPropScaling) (listOf (choose (0, maximumArbitraryFormPropositions))))
    return $ KrM ws r v)
\end{code}

In the instances for pointed models, we want to make sure that the team or world that we're focusing on is actually part of the model. We do that by picking a team or a world respectively as an arbitrary subset or element of the model's worlds.

\begin{code}
instance Arbitrary TeamPointedModel where
  arbitrary = do
    m <- arbitrary
    s <- subsetOf (worlds m)
    return $ TPM m s

instance Arbitrary WorldPointedModel where
  arbitrary = do
    m <- arbitrary
    w <- elements (Set.toList $ worlds m)
    return $ WPM m w
\end{code}

For the Arbitrary instance of the formulas, we check the size parameter: if it is 0, we choose a Proposition to terminate the recursion. Otherwise, we pick one of the other available operators that we can use in an arbitrary formula.

\begin{code}
instance Arbitrary MForm where
  arbitrary = sized arbitraryForm
    where arbitraryForm 0 = MProp <$> choose (1, maximumArbitraryMFormPropositions)
          arbitraryForm _ = oneof [
            MProp <$> choose (1, maximumArbitraryMFormPropositions),
            MNeg <$> f,
            MAnd <$> f <*> f,
            MOr <$> f <*> f,
            MDia <$> f]
          f = scale (`div` arbitraryFormScaling) arbitrary

instance Arbitrary Form where
  arbitrary = sized arbitraryForm
    where arbitraryForm 0 = Prop <$> choose (1, maximumArbitraryMFormPropositions)
          arbitraryForm _ = oneof [
            Prop <$> choose (1, maximumArbitraryFormPropositions),
            pure NE,
            pure Bot,
            Neg <$> f,
            And <$> f <*> f,
            Or <$> f <*> f,
            Dia <$> f]
          f = scale (`div` arbitraryFormScaling) arbitrary

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

r3a, r3b, r3c :: Map World (Set World)
r3a = Map.fromSet (const Set.empty) u3

r3b = Map.fromSet r u3 where
  r 2 = Set.fromList [wp, wpq]
  r 3 = Set.singleton wq
  r _ = Set.empty

r3c = Map.fromSet r u3 where
  r 2 = Set.fromList [wp, wq]
  r _ = Set.empty

v3 :: Map World (Set Proposition)
v3 = Map.fromList [
    (0, Set.singleton 1),
    (1, Set.singleton 2),
    (2, Set.fromList [1,2]),
    (3, Set.empty)
  ]

m3a, m3b, m3c :: KrM
m3a = KrM u3 r3a v3
m3b = KrM u3 r3b v3
m3c = KrM u3 r3c v3

s3a1, s3a2, s3b, s3c :: Team
s3a1 = Set.singleton wq
s3a2 = Set.fromList [wp, wq]
s3b  = Set.fromList [wpq, w0]
s3c  = Set.singleton wpq

-- Lara NarrowScope True
wNSa, wNSb, wNS :: Int
wNSa  = 0
wNSb  = 1
wNS = 2

uNS :: Set World
uNS = Set.fromList [0..2]

rNS :: Map World (Set World)
rNS = Map.fromSet r uNS where
  r 2 = Set.fromList [wNSa, wNSb]
  r _ = Set.empty

vNS :: Map World (Set Proposition)
vNS = Map.fromList [
    (0, Set.singleton 1),
    (1, Set.singleton 2),
    (2, Set.empty)
  ]

mNS :: KrM
mNS = KrM uNS rNS vNS

sNS :: Team
sNS  = Set.singleton wNS

-- Lara NarrowScope False
wNSF1, wNSF2 :: Int
wNSF1 = 2
wNSF2 = 3

uNSF :: Set World
uNSF = Set.fromList [0..3]

rNSF :: Map World (Set World)
rNSF = Map.fromSet r uNSF where
  r 2 = Set.singleton wNSa
  r 3 = Set.singleton wNSb
  r _ = Set.empty

vNSF :: Map World (Set Proposition)
vNSF = Map.fromList [
    (0, Set.singleton 1),
    (1, Set.singleton 2),
    (2, Set.empty),
    (3, Set.empty)
  ]

mNSF :: KrM
mNSF = KrM uNSF rNSF vNSF

sNSF :: Team
sNSF  = Set.fromList [wNSF1, wNSF2]

-- Lara TautologyBoxDEF
wBa, wBb, wBc, wBd, wBbc, wBe, wBcd :: Int
wBa = 0
wBb = 1
wBc = 2
wBd = 3
wBbc = 4
wBe = 5
wBcd = 6

uB :: Set World
uB = Set.fromList [0..6]

rB :: Map World (Set World)
rB = Map.fromSet r uB where
  r 0 = Set.singleton wBd
  r 1 = Set.fromList [wBe, wBd, wBc]
  r 2 = Set.singleton wBc
  r 4 = Set.singleton wBcd
  r _ = Set.empty

vB :: Map World (Set Proposition)
vB = Map.fromList [
    (0, Set.singleton 1),
    (1, Set.singleton 2),
    (2, Set.singleton 3),
    (3, Set.singleton 4),
    (4, Set.fromList [2, 3]),
    (5, Set.singleton 5),
    (6, Set.fromList [3, 4])
  ]


mB :: KrM
mB = KrM uB rB vB

sB :: Team
sB  = Set.fromList [wBa, wBb, wBc]


\end{code}

\begin{code}
-- Basic Modal Logic formulas
data MForm
  = MProp Proposition
  | MNeg MForm
  | MAnd MForm MForm
  | MOr  MForm MForm
  | MDia MForm
  deriving (Eq,Show)

instance Supportable KrM World MForm where
  (m,w) |= MProp n  = n `elem` val' m w
  (m,w) |= MNeg f   = not $ (m,w) |= f
  (m,w) |= MAnd f g = (m,w) |= f && (m,w) |= g
  (m,w) |= MOr f g  = (m,w) |= f || (m,w) |= g
  (m,w) |= MDia f   = any (\v -> (m,v) |= f) $ rel' m w


-- Modal formulas are a subset of BSML-formulas
toBSML :: MForm -> Form
toBSML (MProp n)  = Prop n
toBSML (MNeg f)   = Neg (toBSML f)
toBSML (MAnd f g) = And (toBSML f) (toBSML g)
toBSML (MOr f g)  = Or (toBSML f) (toBSML g)
toBSML (MDia f)   = Dia (toBSML f)

-- In Aloni2024, this is indicated by []+
enrich :: MForm -> Form
enrich (MProp n)  = Prop n `And` NE
enrich (MNeg f)   = Neg (enrich f) `And` NE
enrich (MDia f)   = Dia (enrich f) `And` NE
enrich (MAnd f g) = (enrich f `And` enrich g) `And` NE
enrich (MOr f g)  = (enrich f `Or`  enrich g) `And` NE
\end{code}
