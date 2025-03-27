
\section{models}
\label{sec:models}

We now use the library QuickCheck to randomly generate input for our functions
and test some properties.

\begin{code}
module Models where

import Semantics
import Syntax
import ML

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

\end{code}

\begin{figure}[!ht]
\centering
\resizebox{1\textwidth}{!}{%
\begin{circuitikz}
\tikzstyle{every node}=[font=\LARGE]
\draw  (2.5,13.75) circle (0.5cm) node {\LARGE $w_p$} ;
\draw  (4.25,13.75) circle (0.5cm) node {\LARGE $w_q$} ;
\draw  (2.5,11.5) circle (0.5cm) node {\Large $w_{pq}$} ;
\draw  (4.25,11.5) circle (0.5cm) node {\LARGE $w_{\emptyset}$} ;
\draw [ rounded corners = 3.0] (1.75,14.75) rectangle (5.25,12.75);
\draw [ rounded corners = 3.0] (3.5,14.5) rectangle (5,13);
\draw [ rounded corners = 3.0] (1.5,15) rectangle (5.5,10.75);
\end{circuitikz}
}%
\caption{3a example}
\label{3a}
\end{figure}


\begin{figure}[!ht]
\centering
\resizebox{1\textwidth}{!}{%
\begin{circuitikz}
\tikzstyle{every node}=[font=\LARGE]
\draw  (0.75,14.25) circle (0.5cm) node {\LARGE $w_p$} ;
\draw  (3,14.25) circle (0.5cm) node {\LARGE $w_q$} ;
\draw  (0.75,11.75) circle (0.5cm) node {\Large $w_{pq}$} ;
\draw  (3,11.75) circle (0.5cm) node {\LARGE $w_{\emptyset}$} ;
\draw [ rounded corners = 3.0] (0,10.75) rectangle (3.75,12.5);
\draw [ rounded corners = 3.0] (-0.25,15) rectangle (4,10.5);
\draw [->, >=Stealth] (1.25,11.75) .. controls (2.25,12) and (2,10.5) .. (1,11.25) ;
\draw [->, >=Stealth] (0.75,12.25) -- (0.75,13.75);
\draw [->, >=Stealth] (3,12.25) -- (3,13.75);
\end{circuitikz}
}%
\caption{3b example}
\label{3b}
\end{figure}


\begin{figure}[!ht]
\centering
\resizebox{1\textwidth}{!}{%
\begin{circuitikz}
\tikzstyle{every node}=[font=\LARGE]
\draw  (0.75,14.25) circle (0.5cm) node {\LARGE $w_p$} ;
\draw  (3,14.25) circle (0.5cm) node {\LARGE $w_q$} ;
\draw  (0.75,11.75) circle (0.5cm) node {\Large $w_{pq}$} ;
\draw  (3,11.75) circle (0.5cm) node {\LARGE $w_{\emptyset}$} ;
\draw [ rounded corners = 3.0] (0,11) rectangle (1.5,12.5);
\draw [ rounded corners = 3.0] (-0.25,15) rectangle (3.75,10.75);
\draw [->, >=Stealth] (0.75,12.25) -- (0.75,13.75);
\draw [->, >=Stealth] (0.75,12.25) -- (3,13.75);
\end{circuitikz}
}%
\caption{3c example}
\label{3c}
\end{figure}

\begin{code}

-- Aloni2024 - Figure 3.
w0, wp, wq, wpq :: Int
wp  = 0
wq  = 1
wpq = 2
w0  = 3

u3 :: Set World
u3 = Set.fromList [0..3]

r3a, r3b, r3c :: Map World (Set World)
r3a = Map.fromSet (const Set.empty) u3

r3b = Map.fromSet r u3 where
  r 2 = Set.fromList [wp, wpq]
  r 3 = Set.singleton wq
  r _ = Set.empty

r3c = Map.fromSet r u3 where
  r 2 = Set.fromList [wp, wq]
  r _ = Set.empty

v3 :: Map World (Set Proposition)
v3 = Map.fromList [
    (0, Set.singleton 1),
    (1, Set.singleton 2),
    (2, Set.fromList [1,2]),
    (3, Set.empty)
  ]

m3a, m3b, m3c :: KrM
m3a = KrM u3 r3a v3
m3b = KrM u3 r3b v3
m3c = KrM u3 r3c v3

s3a1, s3a2, s3b, s3c :: Team
s3a1 = Set.singleton wq
s3a2 = Set.fromList [wp, wq]
s3b  = Set.fromList [wpq, w0]
s3c  = Set.singleton wpq




--MOTIVATING EXAMPLE--

wmA, wmB, wmAB, wmE :: Int
wmA = 0
wmB = 1
wmAB = 2
wmE = 3

uM :: Set World
uM = Set.fromList [0..3]

rM :: Map World (Set World)
rM =  Map.fromSet (const Set.empty) uM

vM :: Map World (Set Proposition)
vM = Map.fromList [
    (0, Set.singleton 1),
    (1, Set.singleton 2),
    (2, Set.fromList [1,2]),
    (3, Set.empty)
  ]

mM :: KrM
mM = KrM uM rM vM

sMA, sMB, sMC, sMD :: Team
sMA = Set.fromList [wmA, wmB]
sMB = Set.fromList [wmAB, wmB]
sMC = Set.singleton wmA
sMD = Set.fromList [wmA, wmB, wmE]


\end{code}

