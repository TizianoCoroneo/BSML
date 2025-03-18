
\section{Simple Tests}
\label{sec:simpletests}

We now use the library QuickCheck to randomly generate input for our functions
and test some properties.

\begin{code}
module Main where

import Defs

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.Set as Set

\end{code}

The following uses the HSpec library to define different tests.
Note that the first test is a specific test with fixed inputs.
The second and third test use QuickCheck.

\begin{code}

{--
Properties to test for:
Narrow-scope FC:
\Diamond(\alpha\vee\beta)\vDash\Diamond\alpha\wedge\Diamond\beta

Dual-prohibition:
\neg\Diamond(\alpha\vee\beta)\vDash\neg\Diamond\alpha\wedge\neg\Diamond\beta

Universal FC:
\forall\Diamond(\alpha\vee\beta)\vDash\forall\Diamond\alpha\wedge\Diamond\beta

Wide-scope FC:
\Diamond\alpha\vee\beta\vDash\Diamond\alpha\wedge\Diamond\beta

--}


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
    modifyMaxSize (`div` 10) $ do
    prop "box f <==> !<>!f" $
      \(TPM m s) f -> (m::KrM,s::Team) |= box (f::Form) == (m,s) |= Neg(Dia (Neg f))
    
  describe "Abbreviations" $ do
    prop "strong tautology is always supported" $
      \(TPM m s) -> (m,s) |= toptop
    prop "strong contradiction is never supported" $
      \(TPM m s) -> not $ (m,s) |= botbot
    modifyMaxSize (const 10) $ prop "p v ~p is never supported"  $
      \(TPM m s) -> (m,s) |= (p `Or` Neg p)
    prop "NE v ~NE does *can* be supported" $
      expectFailure $ \(TPM m s) -> (m,s) |= (top `Or` Neg top)
    prop "strong tautology !== top" $
      expectFailure $ \(TPM m s) -> (m,s) |= toptop == (m,s) |= top
  
  describe "Flatness" $
    modifyMaxSize (`div` 10) $ do
    prop "ex.1 (M,s) |= f <==> M,{w} |= f forall w in s (needs Arbitrary MForm)" $
      \(TPM m s) f -> (m,s) |= toBSML (f::MForm) == all (\w -> (m, Set.singleton w) |= toBSML f) s
    prop "M,{w} |= f <==> M,w |= f (needs Arbitrary MForm)" $ 
      \(WPM m w) f -> (m, Set.singleton w) |= toBSML (f::MForm) == (m,w) |= f
  where
    p = Prop 1
    q = Prop 2
    mp = MProp 1
    mq = MProp 2
\end{code}



To run the tests, use \verb|stack test|.

To also find out which part of your program is actually used for these tests,
run \verb|stack clean && stack test --coverage|. Then look for ``The coverage
report for ... is available at ... .html'' and open this file in your browser.
See also: \url{https://wiki.haskell.org/Haskell_program_coverage}.
