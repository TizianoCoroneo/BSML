\begin{code}
module ND_examples where

import Syntax
import ND

-- Given some f, gives a proof of _||_ |- f, as in Lemma 4.2 of Aloni2024.
exFalso :: Form -> Proof
exFalso f = botbotCtr f $ orIntroR Bot $ assume botbot

-- orIntroL is admissible
orIntroL :: Form -> Proof -> Proof
orIntroL g p = orComm $ orIntroR g p

-- Distributivity of And over Or: f & (g v h) ⊢ (f & g) v (f & h)
-- under the condition that f is NE-free
andOrDistr :: Form -> Form -> Form -> Proof
andOrDistr f g h =
  orElim
    (andElimR ass)
    (orIntroR (f `And` h) $ andIntro pf $ assume g)
    (orIntroL (f `And` g) $ andIntro pf $ assume h)
  where
    ass = assume $ f `And` (g `Or` h)
    pf = andElimL ass

\end{code}