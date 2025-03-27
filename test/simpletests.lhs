
\section{Simple Tests}
\label{sec:simpletests}

We now use the library QuickCheck to randomly generate input for our functions
and test some properties.

\begin{code}
module Main where

import Semantics
import Syntax
import ML

import qualified Data.Set as Set
import Test.Hspec

import Models

import Test.Hspec.QuickCheck

\end{code}

The following uses the HSpec library to define different tests.
We use a mix of QuickCheck and specific inputs, depending on what we are testing for.

The "Figure 3" section corresponds to the three examples labeled 3a, 3b, and 3c \cite{Aloni2024}.
The paper gives a couple formulas per example to illustrate the semantics of BSML. We test each of these formulas to confirm our implementation contains the expected semantics.


\begin{code}

main :: IO ()
main = hspec $ do
  describe "Figure 3" $ do
    it "Figure 3b, [<>(p ^ q)]+" $
      (m3b, s3b) |= enrich (MDia (ma `MAnd` mb)) `shouldBe` False
    it "Figure 3c, [<>(p v q)]+" $
      (m3c, s3c) |= enrich (MDia (ma `MOr` mb)) `shouldBe` True

  describe "Motivating Example" $ do
    it "(a) |= (a v b) == True" $
      (mM, sMA) |= toBSML (ma `MOr` mb) `shouldBe` True
    it "(a) |= [a v b]+  == True" $
      (mM, sMA) |= enrich (ma `MOr` mb) `shouldBe` True

    it "(b) |= (a v b) == True" $
      (mM, sMB) |= toBSML (ma `MOr` mb) `shouldBe` True
    it "(b) |= [a v b]+ == True" $
      (mM, sMB) |= enrich (ma `MOr` mb) `shouldBe` True

    it "(c) |= (a v b) == True" $
      (mM, sMC) |= toBSML (ma `MOr` mb) `shouldBe` True
    it "(c) |= [a v b]+ == False" $
      (mM, sMC) |= enrich (ma `MOr` mb) `shouldBe` False

    it "(d) |= (a v b) == False" $
      (mM, sMD) |= toBSML (ma `MOr` mb) `shouldBe` False
    it "(d) |= [a v b]+  == False" $
      (mM, sMD) |= enrich (ma `MOr` mb) `shouldBe` False


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
  describe "NarrowScope" $ do
    it "NarrowScope , <>(a v b)" $
      (mNS, sNS) |= Dia (p `Or` q) `shouldBe` True
    it "NarrowScope , <>a ^ <>b" $
      (mNS, sNS) |= (Dia p `And` Dia q) `shouldBe` True
    it "NarrowScope falsified, <>(a v b)" $
      (mNSF,sNSF) |= Dia (p `Or` q) `shouldBe` True
    it "NarrowScope falsified, (<>a ^ <>b) should be false" $
      (mNSF,sNSF) |= (Dia p `And` Dia q) `shouldBe` False
  describe "Dual-Prohibition" $ do
    prop "Dual-Prohibition , !<>(a v b) |= !<>a ^ !<>b" $
      \(TPM m s) -> (m, s) |= Neg (Dia (p `Or` q)) == (m,s) |= (Neg(Dia p) `And` Neg(Dia q))

  describe "Tautologies" $ 
    modifyMaxSize (`div` 7) $ do
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

  describe "Properties from Paper" $ modifyMaxSize (const 12) $ do
    prop "NarrowScope, <>(a v b) =| (<>a ^ <>b)" $
      \(TPM m s) -> (m,s) |= enrich (MDia (ma `MOr` mb)) == (m,s) |= enrich (MDia ma `MAnd` MDia mb)
    prop "Wide Scope, <>a v <>b) =| <>a ^ <>b" $
      \(TPM m s) -> all (\w -> rel' m w == s) s <= ((m,s)  |= enrich (MDia ma `MOr` MDia mb) <= (m,s) |= enrich (MDia ma `MAnd` MDia mb))

  describe "Flatness" $ modifyMaxSize (const 10) $ do
    prop "(M,s) |= f <==> M,{w} |= f forall w in s" $
      \(TPM m s) f -> (m,s) |= toBSML (f::MForm) == all (\w -> (m, Set.singleton w) |= toBSML f) s
    prop "M,{w} |= f <==> M,w |= f" $
      \(WPM m w) f -> (m, Set.singleton w) |= toBSML (f::MForm) == (m,w) |= f
    prop "Full BSML is *not* flat" $ expectFailure $
      \(TPM m s) f -> (m,s) |= (f::Form) == all (\w -> (m, Set.singleton w) |= f) s

  where
    ma = MProp 1
    mb = MProp 2
\end{code}

To run the tests, use \verb|stack test|.

To also find out which part of your program is actually used for these tests,
run \verb|stack clean && stack test --coverage|. Then look for ``The coverage
report for ... is available at ... .html'' and open this file in your browser.
See also: \url{https://wiki.haskell.org/Haskell_program_coverage}.
