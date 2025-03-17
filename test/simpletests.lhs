
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
\end{code}

The following uses the HSpec library to define different tests.
Note that the first test is a specific test with fixed inputs.
The second and third test use QuickCheck.

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
  describe "Abbreviations" $ do
    prop "strong tautology is always supported" $
      \(m,s) -> (m::KrM, s::Team) |= toptop
    prop "strong contradiction is never supported" $
      \(m,s) -> not $ (m::KrM, s::Team) |= botbot
    modifyMaxSize (const 10) $ prop "p v ~p is never supported"  $
      \(m,s) -> (m::KrM, s::Team) |= (p `Or` Neg p)
    prop "NE v ~NE does *can* be supported" $
      expectFailure $ \(m,s) -> (m::KrM, s::Team) |= (top `Or` Neg top)
    prop "strong tautology !== top" $
      expectFailure $ \(m,s) -> (m::KrM, s::Team) |= toptop == (m,s) |= top
  describe "Flatness" $ do
    xprop "M,s |= f <==> M,{w} |= f forall w in s (needs Arbitrary MForm)" (undefined :: Property)
--      \m s f -> (m::KrM, s::Team) |= toBSML (f::MForm) ==
--        all (\w -> (m, Set.singleton w) |= toBSML f) s
    xprop "M,{w} |= f <==> M,w |= f (needs Arbitrary MForm)" (undefined :: Property)
--      \m w f -> (m::KrM, Set.singleton w) |= toBSML (f::MForm) == (m,w) |= f
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
