This section explains the implementation of Natural Deduction proofs for BSML.
Our code should still be considered \emph{Work in progress}, but already has
useful functionality, with clear potential for future extensions and/or improvements.

Since we want users of this module to solely be able to construct proofs using
the supplied axioms, we explicitly name the exports of this module and omit the
constructor for the \verb|Proof|-type.
\begin{showCode}
module ND
  (
    Proof
  , sorry
  , assume

  -- Rules
  .
  .
  .
  ) where
\end{showCode}

\hide{
\begin{code}
module ND
  (
    Proof
  , sorry
  , assume

  -- Rules for &
  , andIntro
  , andElimL
  , andElimR

  -- Rules for ~
  , negIntro
  , negElim
  , negnegElim
  , dmAnd
  , dmOr
  , neNegElim

  -- Rules for v
  , orIntroR
  , orWkn
  , orComm
  , orAss
  , orElim
  , orMon

  -- Rules for _|_ and NE
  , botElim
  , botbotCtr

  -- Basic modal rules
  , diaMon
  , boxMon
  , diaBoxInter

  -- Rules governing the interaction of the modalities and connectives
  , diaSep
  , diaJoin
  , boxInst
  , boxDiaJoin

  -- Propositional rules involving V/
  , gorIntroL
  , gorIntroR
  , gorElim
  , orGorDistr
  , dmGor
  , neIntro

  -- Modal rules for V/
  , diaGorOrConv
  , boxGorOrConv
  ) where
\end{code}
}

To represent an ND-proof, we use the \verb|Proof|-type, which stores the conclusion
of the proof and all of its open (non-discharged) assumptions.
We use a \verb|Set| to represent proofs to allow easy omission of duplicates and removal (discharges) of
assumptions.
For the sake of simplicity, we decided not to store the entire ND-tree leading to
a conclusion, but it would be a straightforward adaptation to make, if desired.

\begin{code}
import Syntax

import Data.Set (Set)
import qualified Data.Set as Set

-- Type for representing ND-proofs, constructor Prf is for internal use only!
data Proof = Prf {conclusion :: Form,
                  assumptions :: Set Form}
  deriving (Show)

\end{code}

Next, we define a function to represent making a new assumption in a proof; given any
formula $\phi$, it returns the proof with conclusion $\phi$ and open assumptions $\{\phi\}$.

\begin{code}
assume :: Form -> Proof
assume = Prf <*> Set.singleton
\end{code}

Further, we define some functions for convenience.
Here, \verb|sorry| completely subverts our system by creating a proof for any conclusion and set of assumptions,
but it can be useful for users as a placeholder value in proofs.
It is similar to \verb|Lean|'s sorry, and triggers a warning anytime it is
used.
The \verb|hasNE| and \verb|hasGor| functions check whether a formula uses the
\verb|NE| or \verb|Gor| constructor anywhere, which is needed for checking some
side-conditions on ND-rules.
Recall that \verb|hasCr| was defined in \ref{sec:Syntax_plate}.

\begin{code}
sorry :: Form -> Set Form -> Proof
sorry = Prf
{-# WARNING sorry "Proof uses sorry!" #-}

-- Used for checking side-conditions

hasNE :: Form -> Bool
hasNE = hasCr _NE

hasGor :: Form -> Bool
hasGor = hasCr _Gor
\end{code}

Now, we can define all of the rules axiomatizing BSML, as proven in \cite{Aloni2024} (see Chapter 4).
There are quite a lot of rules (32 to be exact, we implemented all of them!), so we will not show all of them here,
but we will highlight a few to give a good idea of the implementations.
\hide{
\begin{code}
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
  | (not . all isBasic) [f, f', g] = error "Cannot apply ~Elim to non-basic formula!"
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
\end{code}
}

Take e.g. the rule $\lor\mathrm{I}$, which introduces $\phi \lor \psi$ from a proof
of $\phi$ under the condition that $\psi$ does not contain \verb|NE|.
This is implemented as:
\begin{code}
-- (c) Rules for v

orIntroR :: Form -> Proof -> Proof
orIntroR g (Prf f ass)
  | hasNE g   = error "Cannot vIntro a formula containing NE!"
  | otherwise = Prf (Or f g) ass
\end{code}

\hide{
\begin{code}
orWkn :: Proof -> Proof
orWkn (Prf f ass) = Prf (Or f f) ass

orComm :: Proof -> Proof
orComm (Prf (Or f g) ass) = Prf (Or g f) ass
orComm _ = error "Cannot apply vCom, conclusion is not a disjunction!"

orAss :: Proof -> Proof
orAss (Prf (f `Or` (g `Or` h)) ass) = Prf ((f `Or` g) `Or` h) ass
orAss _ = error "Cannot apply vAss, conclusion is not a nested disjunction!"
\end{code}
}

For a more complicated example, we can consider $\lor\mathrm{E}$, the rule for
disjunction-elimination:

\begin{code}
orElim :: Proof -> Proof -> Proof -> Proof
orElim (Prf (Or f g) ass) (Prf h ass1) (Prf h' ass2)
  | h /= h'        = error "Cannot apply vElim, conclusions of latter proofs do not match!"
  | any hasNE ass' = error "Cannot apply vElim, latter proofs have undischarged assumptions containing NE!"
  | hasGor h       = error "Cannot apply vElim, latter conclusion contains V/."
  | otherwise      = Prf h $ ass <> ass'
  where ass' = Set.delete f ass1 <> Set.delete g ass2
orElim _ _ _ = error "Cannot apply vElim, conclusion of first proof is not a disjunction!"
\end{code}
Note in particular how we use a combination of guards and pattern matching to ascertain
whether the formulas have the correct form and that all the side-conditions are met.

\hide{
\begin{code}
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
\end{code}
}

For the sake of completeness, we wil also show $\Box\mathrm{Mon}$, a rule involving modal operators and
an interesting side condition:
for every assumptions $\phi$ of the first proof, $\Box\phi$ should be the conclusion
of one of the latter proofs.
Also note how we use \verb|foldMap| to extract the assumptions from each of the latter
proofs and take their union.

\begin{code}
boxMon :: Proof -> [Proof] -> Proof
boxMon (Prf g ass) ps
  | Set.map box ass `Set.isSubsetOf` Set.fromList (map conclusion ps) =
    Prf (box g) $ foldMap assumptions ps
  | otherwise = error "Cannot apply []Mon, former proof has undischarged assumptions!"
\end{code}

\hide{
\begin{code}
diaBoxInter :: Proof -> Proof
diaBoxInter (Prf (Neg (Dia f)) ass) = Prf (box (Neg f)) ass
diaBoxInter _ = error "Cannot apply <>[]Inter,conclusion is not of form ~<>phi!"

-- (f) Rules governing the interaction of the modalities and connectives

diaSep :: Proof -> Proof
diaSep (Prf (Dia (_ `Or` (g `Or` NE))) ass) = Prf (Dia g) ass
diaSep _ = error "Cannot apply <>Sep, conclusion is not of correct form!"

diaJoin :: Proof -> Proof -> Proof
diaJoin (Prf (Dia f) ass1) (Prf (Dia g) ass2) = Prf (Dia (f `Or` g)) $ ass1 <> ass2
diaJoin _ _ = error "Cannot apply <>Join, one of the conclusion is not of the form <>phi!"

boxInst :: Proof -> Proof
boxInst (Prf (Neg (Dia (Neg (f `And` NE)))) ass) = Prf (Dia f) ass
boxInst _ = error "Cannot apply []Inst, conclusion is of correct form!"

boxDiaJoin :: Proof -> Proof -> Proof
boxDiaJoin (Prf (Neg (Dia (Neg f))) ass1) (Prf (Dia g) ass2) = Prf (box (f `Or` g)) $ ass1 <> ass2
boxDiaJoin _ _ = error "Cannot apply []<>Join, conclusions are not of correct form!"

-- (g) Propositional rules involving V/

gorIntroL :: Form -> Proof -> Proof
gorIntroL g (Prf f ass) = Prf (f `Gor` g) ass

gorIntroR :: Form -> Proof -> Proof
gorIntroR g (Prf f ass) = Prf (g `Gor` f) ass

gorElim :: Proof -> Proof -> Proof -> Proof
gorElim (Prf (f `Gor` g) ass) (Prf h ass1) (Prf h' ass2)
  | h /= h'   = error "Cannot apply V/Elim, conclusions of latter proofs do not match!"
  | otherwise = Prf h $ ass <> Set.delete f ass1 <> Set.delete g ass2
gorElim _ _ _ = error "Cannot apply V/Elim, first conclusion is not a global disjunction!"

orGorDistr :: Proof -> Proof
orGorDistr (Prf (f `Or` (g `Gor` h)) ass) = Prf ((f `Or` g) `Gor` (f `Or` h)) ass
orGorDistr _ = error "Cannot apply vV/Distr, conclusion is not of correct form!"

dmGor :: Proof -> Proof
dmGor (Prf (Neg (f `Gor` g)) ass) = Prf (Neg f `And` Neg g) ass
dmGor _ = error "Cannot apply DMV/, conclusion is not of correct form!"

neIntro :: Proof
neIntro = Prf (Bot `Gor` NE) mempty

-- (h) Modal rules for V/

diaGorOrConv :: Proof -> Proof
diaGorOrConv (Prf (Dia (f `Gor` g)) ass) = Prf (Dia f `Or` Dia g) ass
diaGorOrConv _ = error "Cannot apply <>V/vConv, conclusion is not of correct form!"

boxGorOrConv :: Proof -> Proof
boxGorOrConv (Prf (Neg (Dia (Neg (f `Gor` g)))) ass) = Prf (box f `Or` box g) ass
boxGorOrConv _ = error "Cannot apply []V/vConv, conclusion is not of correct form!"
\end{code}
}