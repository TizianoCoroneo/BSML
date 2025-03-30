\subsection{Testing Enrichment and FC}
\label{sec:simpletests}

We also wrote tests for the enriched behavior of our implementation of BSML, to make sure we correctly model FC inferences.

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
The "Figure 3"-section refers to the same figure in \cite{Aloni2024} and tests the associated inferences.
The "Motivating Example" is the model described in the introduction and can be seen in Figure \ref{fig:allMotive}.

\begin{figure}[!h]
    \centering
    \begin{subfigure}[b]{0.23\textwidth}
        \centering
        \resizebox{\linewidth}{!}{%
            \begin{tikzpicture}[every node/.style={font=\LARGE}]
                \draw (2.5,13.75) circle (0.5cm) node {$w_p$};
                \draw (4.25,13.75) circle (0.5cm) node {$w_q$};
                \draw (2.5,11.5) circle (0.5cm) node {$w_{pq}$};
                \draw (4.25,11.5) circle (0.5cm) node {$w_{\emptyset}$};
                \draw [rounded corners=3.0] (1.75,14.75) rectangle (5.25,12.75);
                \draw [rounded corners=3.0] (1.5,15) rectangle (5.5,10.75);
            \end{tikzpicture}%
        }
        \caption{$\vDash(p\vee q)$, $\vDash [p\vee q]^+$}
        \label{fig:Ma}
    \end{subfigure}
    \hfill
    \begin{subfigure}[b]{0.23\textwidth}
        \centering
        \resizebox{\linewidth}{!}{%
            \begin{tikzpicture}[every node/.style={font=\LARGE}]
                \draw (0.75,14.25) circle (0.5cm) node {$w_p$};
                \draw (3,14.25) circle (0.5cm) node {$w_q$};
                \draw (0.75,11.75) circle (0.5cm) node {$w_{pq}$};
                \draw (3,11.75) circle (0.5cm) node {$w_{\emptyset}$};
                \draw [rounded corners=3.0] (0,14.75) rectangle (1.7,10.75);
                \draw [rounded corners=3.0] (-0.25,15) rectangle (4,10.5);
            \end{tikzpicture}%
        }
        \caption{$\vDash(p\vee q)$, $\vDash [p\vee q]^+$}
        \label{fig:Mb}
    \end{subfigure}
    \hfill
    \begin{subfigure}[b]{0.23\textwidth}
        \centering
        \resizebox{\linewidth}{!}{%
            \begin{tikzpicture}[every node/.style={font=\LARGE}]
                \draw (0.75,14.25) circle (0.5cm) node {$w_p$};
                \draw (3,14.25) circle (0.5cm) node {$w_q$};
                \draw (0.75,11.75) circle (0.5cm) node {$w_{pq}$};
                \draw (3,11.75) circle (0.5cm) node {$w_{\emptyset}$};
                \draw [rounded corners=3.0] (2,14.75) rectangle (3.5,13.5);
                \draw [rounded corners=3.0] (-0.25,15) rectangle (3.75,10.75);
            \end{tikzpicture}%
        }
        \caption{$\vDash(p\vee q)$, $\nvDash [p\vee q]^+$}
        \label{fig:Mc}
    \end{subfigure}
    \hfill
    \begin{subfigure}[b]{0.23\textwidth}
        \centering
        \resizebox{\linewidth}{!}{%
            \begin{tikzpicture}[every node/.style={font=\LARGE}]
                \draw  (-8.75,5) circle (0.75cm) node {\LARGE $w_a$} ;
                \draw  (-8.75,2.75) circle (0.75cm) node {\LARGE $w_b$} ;
                \draw  (-6.25,5.5) circle (0.75cm) node {\LARGE $w_{ab}$} ;
                \draw  (-6.5,2.75) circle (0.75cm) node {\LARGE $w_{\emptyset}$} ;
                \draw [short] (-9.75,5.5) -- (-9.75,1.75);
                \draw [short] (-9.75,1.75) -- (-6,1.75);
                \draw [short] (-6,1.75) .. controls (-5.5,1.75) and (-5,2.75) .. (-6,3.75);
                \draw [short] (-6,3.75) -- (-8,5.75);
                \draw [short] (-9.75,5.5) .. controls (-9.5,6.25) and (-8.5,6.25) .. (-8,5.75);
                \draw [rounded corners = 3] (-10,6.5) rectangle (-5.25,1.5);
            \end{tikzpicture}%
        }
        \caption{$\vDash(p\vee q)$, $\vDash [p\vee q]^+$}
        \label{fig:Md}
    \end{subfigure}
    \caption{}
    \label{fig:allMotive}
\end{figure}
Narrow-scope and wide-scope relate to the different kinds of FC-inference that Aloni describes and these tests confirm that FC-inference holds in general for our implementation.
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

To run all the tests, use \verb|stack test| or \verb|make test| to also see the coverage report.
