
\section{Simple Tests}
\label{sec:simpletests}

We now use the library QuickCheck to randomly generate input for our functions
and test some properties.

\begin{code}
module Main where

import Defs

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.Set as Set

\end{code}

The following uses the HSpec library to define different tests. 
We use a mix of QuickCheck and specific inputs, depending on what we are testing for.

The "Figure 3" section corresponds to the three examples labeled 3a, 3b, and 3c \cite{Aloni2024}. 
The paper gives a couple formulas per example to illustrate the semantics of BSML. We test each of these formulas 
to confirm our implementation contains the expected semantics.

\begin{code}

main :: IO ()
main = hspec $ do
  describe "Figure 3" $ do
    it "Figure 3b, [<>(p ^ q)]+" $
      (m3b, s3b) |= enrich (MDia (mp `MOr` mq)) `shouldBe` False
    it "Figure 3c, [<>(p v q)]+" $
      (m3c, s3c) |= enrich (MDia (mp `MOr` mq)) `shouldBe` True

\end{code}


The paper \cite{Aloni2024} discusses various properties that must hold for our implementation. 
Narrow-scope and wide-scope relate to the "pragmatic enrichment function." 
The flatness test confirms that our implementation of BSML formulas are flat.

\begin{code}

  describe "Properties from Paper" $ 
    modifyMaxSize (`div` 7) $ do
    prop "NarrowScope, <>(a v b) =| (<>a ^ <>b)" $
      \(TPM m s) -> (m,s) |= enrich (MDia (mp `MOr` mq)) == (m,s) |= enrich (MDia mp `MAnd` MDia mq)
    prop "Wide Scope, <>a v <>b) =| <>a ^ <>b" $
      \(TPM m s) -> all (\w -> rel' m w == s) s <= ((m,s)  |= enrich (MDia mp `MOr` MDia mq) <= (m,s) |= enrich (MDia mp `MAnd` MDia mq))
  describe "Flatness" $
    modifyMaxSize (`div` 10) $ do
    prop "ex.1 (M,s) |= f <==> M,{w} |= f forall w in s (needs Arbitrary MForm)" $
      \(TPM m s) f -> (m,s) |= toBSML (f::MForm) == all (\w -> (m, Set.singleton w) |= toBSML f) s
    prop "M,{w} |= f <==> M,w |= f (needs Arbitrary MForm)" $ 
      \(WPM m w) f -> (m, Set.singleton w) |= toBSML (f::MForm) == (m,w) |= f
  where
    mp = MProp 1
    mq = MProp 2
\end{code}



To run the tests, use \verb|stack test|.

To also find out which part of your program is actually used for these tests,
run \verb|stack clean && stack test --coverage|. Then look for ``The coverage
report for ... is available at ... .html'' and open this file in your browser.
See also: \url{https://wiki.haskell.org/Haskell_program_coverage}.
