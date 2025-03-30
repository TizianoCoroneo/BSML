To define the \emph{pragmatic enrichment} function mentioned in the introduction,
we define a type to represent formulas of basic Modal Logic (ML) and implement
the standard Kripke semantics.
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module ML where

import Syntax
import Semantics

import Test.QuickCheck

-- Basic Modal Logic formulas
data MForm
  = MProp Proposition
  | MNeg MForm
  | MAnd MForm MForm
  | MOr  MForm MForm
  | MDia MForm
  deriving (Eq,Show)

-- Kripke semantics
instance Supportable KrM World MForm where
  (m,w) |= MProp n  = n `elem` val' m w
  (m,w) |= MNeg f   = not $ (m,w) |= f
  (m,w) |= MAnd f g = (m,w) |= f && (m,w) |= g
  (m,w) |= MOr f g  = (m,w) |= f || (m,w) |= g
  (m,w) |= MDia f   = any (\v -> (m,v) |= f) $ rel' m w
\end{code}

Formally, pragmatic enrichment is given by the function $[\cdot]^+ \colon \mathrm{ML} \to \mathrm{BSML}$,
defined as
\begin{align*}
    [p]^+ &\coloneqq p \land \texttt{NE} && \text{for } p \in \texttt{Prop}\\
    [\heartsuit \phi]^+ &\coloneqq \heartsuit [\phi]^+ \land \texttt{NE} && \text{for } \heartsuit \in \{\lnot, \lozenge\} \\
    [\phi \odot \psi]^+ &\coloneqq ([\phi]^+ \odot [\psi]^+) \land \texttt{NE} && \text{for } \odot \in \{\lor, \land\}
\end{align*}
which is straightforward to implement in Haskell.
\begin{code}
-- The pragmatic enrichment function [.]+ : ML -> BSML.
enrich :: MForm -> Form
enrich (MProp n)  = Prop n `And` NE
enrich (MNeg f)   = Neg (enrich f) `And` NE
enrich (MDia f)   = Dia (enrich f) `And` NE
enrich (MAnd f g) = (enrich f `And` enrich g) `And` NE
enrich (MOr f g)  = (enrich f `Or`  enrich g) `And` NE
\end{code}
To test the semantic effect of enrichment, and some other properties of ML as a fragment of BSML,
we also implement the canonical embedding of ML into BSML.
\begin{code}
-- Embedding ML >-> BSML
toBSML :: MForm -> Form
toBSML (MProp n)  = Prop n
toBSML (MNeg f)   = Neg (toBSML f)
toBSML (MAnd f g) = And (toBSML f) (toBSML g)
toBSML (MOr f g)  = Or (toBSML f) (toBSML g)
toBSML (MDia f)   = Dia (toBSML f)
\end{code}

As the reader should expect at this point, we also implement an arbitrary instance
for formulas of ML, which is completely analogous to that for BSML:
\begin{code}
randomMProp :: Gen MForm
randomMProp = MProp <$> choose (1, numProps)

instance Arbitrary MForm where
  arbitrary = sized $ \case
    0 -> randomMProp
    _ -> oneof [
      randomMProp,
      MNeg <$> f,
      MAnd <$> f <*> f,
      MOr  <$> f <*> f,
      MDia <$> f]
    where f = scale (`div` 2) arbitrary

  shrink (MNeg f)     = f        :  [MNeg f'      | f'        <- shrink f]
  shrink (MDia f)     = f        :  [MDia f'      | f'        <- shrink f]
  shrink (MAnd f1 f2) = [f1, f2] ++ [MAnd f1' f2' | (f1',f2') <- shrink (f1,f2)]
  shrink (MOr f1 f2)  = [f1, f2] ++ [MOr  f1' f2' | (f1',f2') <- shrink (f1,f2)]
  shrink _            = []
\end{code}