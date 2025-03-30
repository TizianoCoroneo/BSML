
We now use the library QuickCheck to randomly generate input for our functions
and test some properties.

\hide{\begin{code}
module Main where

import Test.Hspec ( hspec, describe, it, shouldBe )
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Semantics
import Syntax
import Models
\end{code}}


The following uses the HSpec library to define different tests.
We use a mix of QuickCheck and specific inputs, depending on what we are testing for.

The "Figure 3" section corresponds to the three examples labeled 3a, 3b, and 3c \cite{Aloni2024}.
The paper gives a couple formulas per example to illustrate the semantics of BSML. We test each of these formulas
to confirm our implementation contains the expected semantics.

The "Figure 3" section corresponds to the three examples labeled 3a, 3b, and 3c in Figure 1 and in the paper \cite{Aloni2024}. 
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
    it "Figure 3c, <>(p v q)" $
      (m3c, s3c) |= Dia (p `Or` q) `shouldBe` True
\end{code}

Below we test the tautologies that should hold for BSML logic ensuring our implementation is correct.
Here we use QuickCheck, but we need to limit the maximal size of the arbitrary models we generate.
This is necessary because the evaluation of support in team semantics is inherently exponential 
in complexity (see e.g. the clause for support of disjunctions).

\begin{code}
  describe "Tautologies" $ modifyMaxSize (const 20) $ do
    modifyMaxSize (const 10) $ prop "box f <==> !<>!f" $
      \(TPM m s) f -> (m,s) |= box (f::Form) == (m,s) |= Neg(Dia (Neg f))

    prop "<>(p v q) <==> <>p v <>q" $
      \(TPM m s) -> (m,s) |= Dia (p `Or` q) == (m,s) |= (Dia p `Or` Dia q)
    prop "<>(p ^ q) ==> <>p ^ <>q" $
      \(TPM m s) -> (m,s) |= Dia (p `And` q) ==> (m,s) |= (Dia p `And` Dia q)
    modifyMaxSize (const 25) $ prop "<>p ^ <>q !==> <>(p ^ q)" $
      expectFailure $ \(TPM m s) -> (m,s) |= (Dia p `And` Dia q) ==> (m,s) |= Dia (p `And` q)

    prop "box(p ^ q) <==> box p ^ box q" $
      \(TPM m s) -> (m,s) |= box (p `And` q) == (m,s) |= (box p `And` box q)
    prop "box p v box q ==> box(p v q)" $
      \(TPM m s) -> (m,s) |= (box p `Or` box q) ==> (m,s) |= box (p `Or` q)
    modifyMaxSize (const 25) $ modifyMaxSuccess (const 1000) $ prop "box(p v q) !==> box p v box q" $
      expectFailure $ \(TPM m s) -> (m,s) |= box (p `Or` q) ==> (m,s) |= (box p `Or` box q)

    prop "DeMorgan's Law" $
      \(TPM m s) -> (m,s) |= Neg (p `And` q)== (m,s) |= (Neg p `Or` Neg q)

    prop "Dual-Prohibition, !<>(a v b) |= !<>a ^ !<>b" $
      \(TPM m s) -> (m, s) |= Neg (Dia (p `Or` q)) == (m,s) |= (Neg(Dia p) `And` Neg(Dia q))
    prop "strong tautology is always supported" $
      \(TPM m s) -> (m,s) |= toptop
    prop "strong contradiction is never supported" $
      \(TPM m s) -> not $ (m,s) |= botbot
    modifyMaxSize (const 10) $ prop "p v ~p is never supported"  $
      \(TPM m s) -> (m,s) |= (p `Or` Neg p)
    prop "NE v ~NE *can* be supported" $
      expectFailure $ \(TPM m s) -> not $ (m,s) |= (top `Or` Neg top)
    prop "strong tautology !== top" $
      expectFailure $ \(TPM m s) -> (m,s) |= toptop == (m,s) |= top
    where
        p = Prop 1
        q = Prop 2
\end{code}