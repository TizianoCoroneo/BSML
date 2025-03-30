\subsection{Testing Enrichment and FC}
\label{sec:simpletests}

We can use the library QuickCheck to test the enriched behavior of our implementation of BSML, to make sure we correctly implemented FC.

\hide{
\begin{code}
module Main where

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

import Semantics
import Parser
import Syntax
import ML
import Models ( m3b, m3c, mM, s3b, s3c, sMA, sMB, sMC, sMD )

\end{code}
}

The following uses the HSpec library to define different tests.
We use a mix of QuickCheck and specific inputs, depending on what we are testing for.

The "Figure 3" section in the tests corresponds to Figure 1 described above in a previous section of the paper. The "Motivating Example" uses the Figure 2 models in order to show our disjunction behaves as expected.
The paper \cite{Aloni2024} describes some formulas that make use of enrichment that must hold in these models.


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

    prop "Arbitrary |= (a v b) !== Arbitrary |= [a v b]+  == False" $
      expectFailure $ \(TPM m s) -> (m,s) |= toBSML (ma `MOr` mb) == (m,s) |= enrich (ma `MOr` mb)

\end{code}

Narrow-scope and wide-scope relate to the "pragmatic enrichment function," and these tests confirm that FC-inference holds in our implementation.
The flatness test confirms that our implementation of team semantics is flat on ML-formulas.

\begin{code}
  describe "Properties from Paper" $ modifyMaxSize (const 25) $ do
    prop "NarrowScope, <>(a v b) =| (<>a ^ <>b)" $
      \(TPM m s) -> (m,s) |= enrich (MDia (ma `MOr` mb)) == (m,s) |= enrich (MDia ma `MAnd` MDia mb)
    prop "Wide Scope, <>a v <>b) =| <>a ^ <>b" $
      \(TPM m s) -> all (\w -> rel' m w == s) s <= ((m,s)  |= enrich (MDia ma `MOr` MDia mb) <= (m,s) |= enrich (MDia ma `MAnd` MDia mb))

  describe "Flatness" $ modifyMaxSize (const 15) $ do
    modifyMaxSize (const 10) $ prop "(M,s) |= f <==> M,{w} |= f forall w in s" $
      \(TPM m s) f -> (m,s) |= toBSML (f::MForm) == all (\w -> (m, [w]) |= toBSML f) s
    prop "M,{w} |= f <==> M,w |= f" $
      \(WPM m w) f -> (m, [w]) |= toBSML (f::MForm) == (m,w) |= f
    prop "Full BSML is *not* flat" $ expectFailure $
      \(TPM m s) f -> (m,s) |= (f::Form) == all (\w -> (m, [w]) |= f) s


\end{code}

We also test our ability to correctly parse and pretty print, however the specifics of these functions are explained in the following sections.

\begin{code}
  describe "Pretty Print and Parsing" $ do
    prop "formula -> prettyPrint -> Parsing == original formula" $
      \f -> parseFormula (ppForm (f::Form)) == Right f
  where
    ma = MProp 1
    mb = MProp 2
\end{code}

To run the all the tests, use \verb|stack test|.
