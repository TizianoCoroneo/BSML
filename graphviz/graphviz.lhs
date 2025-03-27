\section{Graph Viz}\label{sec:viz}

\begin{code}
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Commands

toGraph :: Tableau -> DotGraph String
toGraph t = digraph' $ toGraph' t where
  toGraph' :: Tableau -> Dot String
  toGraph' (T fs ts) = do
    let root = intercalate "," $ map pForm fs
    node root []
    mapM_ toGraph' ts -- but now we still need edges! use "-->"

showTab :: Tableau -> IO ()
showTab t = runGraphvizCanvas' (toGraph t) Xlib

proveAndShow :: Form -> IO ()
proveAndShow = showTab . tabFor . return . Neg
\end{code}