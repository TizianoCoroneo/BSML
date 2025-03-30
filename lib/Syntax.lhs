\subsection{Syntax}
\label{sec:BSML_syntax}
\begin{code}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module Syntax where

import Data.Data (Data)

import Control.Lens
import Control.Lens.Extras (is)

import Test.QuickCheck
\end{code}
This module describes the syntactical elements of BSML.
We define formulas, some syntactical operations and provide generators for random formulas.

The formulas of BSML are defined as follows:
\begin{code}
type Proposition = Int

-- Formulas of BSML.
data Form
  = Bot
  | NE
  | Prop Proposition
  | Neg Form
  | And Form Form
  | Or  Form Form
  | Gor Form Form
  | Dia Form
  deriving (Eq,Ord,Show,Data)
\end{code}
Readers familiar with Modal Logic (see e.g.\cite{BdRV}) should recognize this as
the basic modal language, extended with \verb|NE|, the nonemptiness atom, and
$\gor$, global disjunction.
As we will see when defining the semantics, \verb|NE|is used to exclude the
assertion of logical statements due to empty information-configurations (i.e. empty states/teams).

For the sake of more legible output, we also define a pretty-printer for formulas.
Note that we print $\gor$ as \verb|V/| rather than \verb|\V| to avoid having to
worry about the escape character.
\begin{code}
ppForm :: Form -> String
ppForm = \case
  Bot       -> "_|_"
  NE        -> "NE"
  Prop p    -> show p
  Neg f     -> "~" ++ ppForm f
  And f1 f2 -> "(" ++ ppForm f1 ++ " & " ++ ppForm f2 ++ ")"
  Or f1 f2  -> "(" ++ ppForm f1 ++ " v " ++ ppForm f2 ++ ")"
  Gor f1 f2 -> "(" ++ ppForm f1 ++ " V/ " ++ ppForm f2 ++ ")"
  Dia f     -> "<>" ++ ppForm f
\end{code}

Further, we define some abbreviations for formulas, following \cite{Aloni2024}.
As usual, $\Box$ is defined as the dual of $\lozenge$.
As will become evident when we define the semantics, $\bot$ is supported in a state if and only if that state is empty.
It is therefore referred to as the \textit{weak contradiction}.
The strong contradiction, defined as $\botbot \coloneqq \bot \land \texttt{NE}$, will never be supported.
Dually, the formula $\top \coloneqq \texttt{NE}$ serves as the \textit{weak tautology}, being supported by non-empty states
and the \texttt{strong tautology} $\toptop \coloneqq \neg \bot$ is always supported.
\begin{code}
-- Define box as the dual of diamond.
box :: Form -> Form
box = Neg . Dia . Neg

-- Define the strong contradiction (which is never assertable).
botbot :: Form
botbot = And Bot NE

-- NE functions as a weak tautology (assertable in non-empty states).
top :: Form
top = NE

-- Define the strong tautology (which is always assertable).
toptop :: Form
toptop = Neg Bot
\end{code}

As in \cite{Aloni2024}, we can use these notions of contradiction and tautology
to interpret finite (global) disjunctions and conjunctions:

\begin{code}
bigOr :: [Form] -> Form
bigOr [] = Bot
bigOr fs = foldr1 Or fs

bigAnd :: [Form] -> Form
bigAnd [] = toptop
bigAnd fs = foldr1 And fs

bigGor :: [Form] -> Form
bigGor [] = botbot
bigGor fs = foldr1 Gor fs
\end{code}

Note that we could have (semantically equivalently) defined e.g.
\begin{showCode}
bigor :: [Form] -> Form
bigor = foldr Or Bot
\end{showCode}
but this would have had the undesired side-effect of including Bot in \emph{every}
disjunction, including non-empty ones.

\subsubsection{Random formulas}
In order to verify some properties of BSML, we would like to be able to generate
random formulas. We will use QuickCheck's ecosystem for this purpose, so we only
need to define an instance of \verb|Arbitrary| for \verb|Form|.

First, we fix a number of propositions, which we will also use when generating
random valuations for models.
This guarantees that random models and formulas have a more meaningful interaction,
in the sense that the formulas will actually refer to the propositions that occur in the model.
\begin{code}
-- We use proposition in the range (1, numProps).
numProps :: Int
numProps = 32
\end{code}
One might wonder why we do not use the size parameter of a generator to determine the range of propositions.
We intentionally avoid this, because it would introduce a bias in the occurence of
Propositions, where more nested subformulas will not contain high propositions.

Now we can define the \verb|Arbitrary Form|-instance using a standard sized generator,
where formulas generated with size $0$ are random atoms and larger formulas are
generated by applying a random constructor to random smaller formulas.
\begin{code}
-- Generate a random atom.
randomAtom :: Gen Form
randomAtom = oneof [Prop <$> choose (1, numProps), pure NE, pure Bot]

instance Arbitrary Form where
  arbitrary = sized $ \case
    0 -> randomAtom
    _ ->  oneof [
        randomAtom,
        Neg <$> f,
        And <$> f <*> f,
        Or  <$> f <*> f,
        Gor <$> f <*> f,
        Dia <$> f
      ]
    where f = scale (`div` 2) arbitrary
\end{code}
The choice to scale the size of the generator by dividing it by 2 is completely arbitrary,
but seems to work well in practice and is used in similar projects, see e.g. \cite{gattinger2024:software:SMCDEL}.

Last, we also define shrinks of formulas that empower QuickCheck to attempt
simplifying counterexamples when/if it finds any.
\begin{code}
  shrink (Neg f)     = [Bot, NE, f]      ++ [Neg f'      | f'        <- shrink f]
  shrink (Dia f)     = [Bot, NE, f]      ++ [Dia f'      | f'        <- shrink f]
  shrink (And f1 f2) = [Bot, NE, f1, f2] ++ [And f1' f2' | (f1',f2') <- shrink (f1,f2)]
  shrink (Or f1 f2)  = [Bot, NE, f1, f2] ++ [Or  f1' f2' | (f1',f2') <- shrink (f1,f2)]
  shrink (Gor f1 f2) = [Bot, NE, f1, f2] ++ [Gor f1' f2' | (f1',f2') <- shrink (f1,f2)]
  shrink _           = []
\end{code}

\subsubsection{Boilerplate for subformulas}
\label{sec:Syntax_plate}
This section is slightly technical and can safely be skipped.
It introduces some functions that allow us to check properties of subformulas (e.g. whether a formula contains \verb|NE| anywhere in its subformulas).

The \verb|Lens|-library defines a typeclass \verb|Plated| that implements a lot
of boilerplate code for the transitive descendants of values of recursively defined types,
so let us make \verb|Form| an instance.
\begin{code}
-- Use default implementation: plate = uniplate
instance Plated Form
\end{code}

We also derive a prism corresponding to every constructor of \verb|Form|.
\begin{code}
-- Derives prisms _Bot, _NE, ..., _Gor, _Dia
makePrisms ''Form
\end{code}
For readers unfamiliar with this, a\verb|Prism| is to a constructors what a \verb|Lens| is to a field.
For example, the derived \verb|Prism| for \verb|Or| is of type
\begin{verbatim}
_Or :: Prism' Form (Form, Form)
\end{verbatim}
which can loosely be interpreted as a pair of functions; one that turns a
\verb|(Form, Form)| into a \verb|Form| (by applying \verb|Or| in this case), and one that
tries to turn a \verb|Form| into a \verb|(Form, Form)|
(in this case, by taking the arguments out of the constructor if the formula is a disjunction).
As we are familiar with from \verb|Lens|, this is suitably generalized.

Now, we can e.g. check whether a formula uses the constructors \verb|NE| or \verb|Gor|
by seeing if any of its transitive descendants is built using one of these constructors.
\begin{code}
isBasic :: Form -> Bool
isBasic = any ((||) . is _NE <*> is _Gor) . universe
\end{code}
Or more generally, check whether a certain constructor was used.
\begin{code}
hasCr :: Prism' Form fs -> Form -> Bool
hasCr = (. universe) . any . is
\end{code}
