
\section{Simple Tests}
\label{sec:simpletests}

We now use the library QuickCheck to randomly generate input for our functions
and test some properties.

\begin{code}
module Main where

import Defs

import qualified Data.Set as Set

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.Set as Set

\end{code}

The following uses the HSpec library to define different tests.
We use a mix of QuickCheck and specific inputs, depending on what we are testing for.

The "Figure 3" section corresponds to the three examples labeled 3a, 3b, and 3c \cite{Aloni2024}.
The paper gives a couple formulas per example to illustrate the semantics of BSML. We test each of these formulas to confirm our implementation contains the expected semantics.


\begin{code}
main :: IO ()
main = hspec $ do
  describe "Figure 3" $ do
    it "Figure 3a1, p v q" $
      (m3a, s3a1) |= (p `Or` q) `shouldBe` True
    it "Figure 3a1, (p ^ NE) v (q ^ NE)" $
      (m3a, s3a1) |= (And p NE `Or` And q NE) `shouldBe` False
    it "Figure 3a2, (p ^ NE) v (q ^ NE)" $
      (m3a, s3a2) |= (And p NE `Or` And q NE) `shouldBe` True
    it "Figure 3b, <>q" $
      (m3b, s3b) |= Dia q `shouldBe` True
    it "Figure 3b, <>p" $
      (m3b, s3b) |= Dia p `shouldBe` False
    it "Figure 3b, []q" $
      (m3b, s3b) |= box q `shouldBe` False
    it "Figure 3b, []p v []q" $
      (m3b, s3b) |= (box p `Or` box q) `shouldBe` True
    it "Figure 3b, <>p ^ <>q" $
      (m3b, s3b) |= (Dia p `And` Dia q) `shouldBe` False
    it "Figure 3b, [<>(p ^ q)]+" $
      (m3b, s3b) |= enrich (MDia (mp `MOr` mq)) `shouldBe` False
    it "Figure 3c, <>(p v q)" $
      (m3c, s3c) |= (Dia p `Or` Dia q) `shouldBe` True
    it "Figure 3c, [<>(p v q)]+" $
      (m3c, s3c) |= enrich (MDia (mp `MOr` mq)) `shouldBe` True

\end{code}

We will expand this section later, by adding more tautologies that should hold for BSML logic ensuring our implementation is correct.
Here we use QuickCheck, but we need to limit the maximal size of the arbitrary models we generate.
This is necessary because the evaluation of support in team semantics is inherently exponential in complexity (see e.g. the clause for support of disjunctions).

\begin{code}
  describe "Tautologies" $ modifyMaxSize (`div` 10) $ do
    prop "box f <==> !<>!f" $
      \(TPM m s) f -> (m::KrM,s::Team) |= box (f::Form) == (m,s) |= Neg(Dia (Neg f))
    prop "Dual-Prohibition, !<>(a v b) |= !<>a ^ !<>b" $
      \(TPM m s) -> (m, s) |= Neg (Dia (p `Or` q)) == (m,s) |= (Neg(Dia p) `And` Neg(Dia q))
    prop "strong tautology is always supported" $
      \(TPM m s) -> (m,s) |= toptop
    prop "strong contradiction is never supported" $
      \(TPM m s) -> not $ (m,s) |= botbot
    prop "p v ~p is never supported"  $
      \(TPM m s) -> (m,s) |= (p `Or` Neg p)
    prop "NE v ~NE does *can* be supported" $
      expectFailure $ \(TPM m s) -> (m,s) |= (top `Or` Neg top)
    prop "strong tautology !== top" $
      expectFailure $ \(TPM m s) -> (m,s) |= toptop == (m,s) |= top

\end{code}

The paper \cite{Aloni2024} discusses various interesting properties that should hold for our implementation.
Narrow-scope and wide-scope relate to the "pragmatic enrichment function."
The flatness test confirms that our implementation of ML formulas are flat.

\begin{code}

  describe "Properties from Paper" $ modifyMaxSize (const 10) $ do
    prop "NarrowScope, <>(a v b) =| (<>a ^ <>b)" $
      \(TPM m s) -> (m,s) |= enrich (MDia (mp `MOr` mq)) == (m,s) |= enrich (MDia mp `MAnd` MDia mq)
    prop "Wide Scope, <>a v <>b) =| <>a ^ <>b" $
      \(TPM m s) -> all (\w -> rel' m w == s) s <= ((m,s)  |= enrich (MDia mp `MOr` MDia mq) <= (m,s) |= enrich (MDia mp `MAnd` MDia mq))
  describe "Flatness" $ modifyMaxSize (const 10) $ do
    prop "(M,s) |= f <==> M,{w} |= f forall w in s" $
      \(TPM m s) f -> (m,s) |= toBSML (f::MForm) == all (\w -> (m, Set.singleton w) |= toBSML f) s
    prop "M,{w} |= f <==> M,w |= f" $
      \(WPM m w) f -> (m, Set.singleton w) |= toBSML (f::MForm) == (m,w) |= f
    prop "Full BSML is *not* flat" $ expectFailure $
      \(TPM m s) f -> (m,s) |= (f::Form) == all (\w -> (m, Set.singleton w) |= f) s
  where
    p = Prop 1
    q = Prop 2
    mp = MProp 1
    mq = MProp 2
\end{code}