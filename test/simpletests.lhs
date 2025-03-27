
\section{Simple Tests}
\label{sec:simpletests}

We now use the library QuickCheck to randomly generate input for our functions
and test some properties.

\begin{code}
module Main where

import Defs

import Models

import Test.Hspec ( hspec, describe, it, shouldBe )
import Test.Hspec.QuickCheck

import qualified Data.Set as Set

\end{code}

The following uses the HSpec library to define different tests. 
We use a mix of QuickCheck and specific inputs, depending on what we are testing for.

The "Figure 3" section corresponds to the examples 3b and 3c \cite{Aloni2024}. 
The paper gives a couple formulas per example to illustrate the semantics of BSML. We test the formulas that use enrich 
to confirm our implementation has the expected behavior.

\begin{code}

main :: IO ()
main = hspec $ do
  describe "Figure 3" $ do
    it "Figure 3b, [<>(p ^ q)]+" $
      (m3b, s3b) |= enrich (MDia (ma `MOr` mb)) `shouldBe` False
    it "Figure 3c, [<>(p v q)]+" $
      (m3c, s3c) |= enrich (MDia (ma `MOr` mb)) `shouldBe` True

  describe "Motivating Example" $ do
    it "(a) |= (a v b) == True" $
      (mM, sMA) |= toBSML (ma `MAnd` mb) `shouldBe` True
    it "(a) |= [a v b]+  == True" $
      (mM, sMA) |= (a `And` b) `shouldBe` True

    it "(b) |= (a v b) == True" $
      (mM, sMB) |= toBSML (ma `MAnd` mb) `shouldBe` True
    it "(b) |= [a v b]+ == True" $
      (mM, sMB) |= (a `And` b) `shouldBe` True

    it "(c) |= (a v b) == True" $
      (mM, sMC) |= toBSML (ma `MAnd` mb) `shouldBe` True
    it "(c) |= [a v b]+ == False" $
      (mM, sMC) |= (a `And` b) `shouldBe` False

    it "(d) |= (a v b) == False" $
      (mM, sMD) |= toBSML (ma `MAnd` mb) `shouldBe` False
    it "(d) |= [a v b]+  == False" $
      (mM, sMD) |= (a `And` b) `shouldBe` False


\end{code}


The paper \cite{Aloni2024} discusses various properties that must hold for our implementation. 
Narrow-scope and wide-scope relate to the "pragmatic enrichment function." 
The flatness test confirms that our implementation of BSML formulas are flat.

\begin{code}

  describe "Properties from Paper" $ 
    modifyMaxSize (const 12) $ do
    prop "NarrowScope, <>(a v b) =| (<>a ^ <>b)" $
      \(TPM m s) -> (m,s) |= enrich (MDia (ma `MOr` mb)) == (m,s) |= enrich (MDia ma `MAnd` MDia mb)
    prop "Wide Scope, <>a v <>b) =| <>a ^ <>b" $
      \(TPM m s) -> all (\w -> rel' m w == s) s <= ((m,s)  |= enrich (MDia ma `MOr` MDia mb) <= (m,s) |= enrich (MDia ma `MAnd` MDia mb))
  describe "Flatness" $
    modifyMaxSize (const 10) $ do
    prop "ex.1 (M,s) |= f <==> M,{w} |= f forall w in s (needs Arbitrary MForm)" $
      \(TPM m s) f -> (m,s) |= toBSML (f::MForm) == all (\w -> (m, Set.singleton w) |= toBSML f) s
    prop "M,{w} |= f <==> M,w |= f (needs Arbitrary MForm)" $ 
      \(WPM m w) f -> (m, Set.singleton w) |= toBSML (f::MForm) == (m,w) |= f
  where
    a = Prop 1
    b = Prop 2
    ma = MProp 1
    mb = MProp 2
\end{code}



To run the tests, use \verb|stack test|.

To also find out which part of your program is actually used for these tests,
run \verb|stack clean && stack test --coverage|. Then look for ``The coverage
report for ... is available at ... .html'' and open this file in your browser.
See also: \url{https://wiki.haskell.org/Haskell_program_coverage}.
