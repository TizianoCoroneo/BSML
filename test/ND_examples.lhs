\subsection{Some examples}
As mentioned before, the implementation of natural deduction is still a \emph{Work in progress},
but in this section, we will quickly illustrate some of the functionality we already have.

\begin{code}
module ND_examples where

import Syntax
import ND
\end{code}

The most obvious functionality is obvious: representing proofs!
For example, here is the proof for ex falso in our language:
\begin{code}
-- Given some f, gives a proof of _||_ |- f, as in Lemma 4.2 of Aloni2024.
exFalso :: Form -> Proof
exFalso f = botbotCtr f $ orIntroR Bot $ assume botbot
\end{code}
One of the drawbacks of our language is that the type signature of proofs is
not very illuminating as to their content. However, note that the information can
be retrieved by inspecting the proof:
\begin{verbatim}
> ppProof $ exFalso $ Prop 1
"(_|_ & NE) |- 1"
\end{verbatim}
For a more involved proof, consider the following implementation of the ND-proof
for distributivity of $\land$ over $\lor$:
\begin{code}
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
While this gives a structured way of writing proofs that are guaranteed to be sound,
it is still missing quite some functionality one would expect from an interactive theorem
prover: for example, automatically filling in holes in a proof using \verb|sorry|.

Apart from representing proofs, we can also create new functions between proofs from our axioms.
These represent admissible rules in the system, for example:
\begin{code}
-- orIntroL is admissible
orIntroL :: Form -> Proof -> Proof
orIntroL = (orComm .) . orIntroR
\end{code}
shows that $\lor\mathrm{I}$ on the left (instead of the right, as in the axiomatization),
is admissible.