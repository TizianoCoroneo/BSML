\section{An Executable Function}\label{sec:Main}

We now describe a very simple executable function. Upon writing \verb|main| in ghci, the function asks the user to provide a BSML formula as input. 
Using the parser, we parse this string, and try to come up with an example that does not satisfy the formula. 

Note that as with any other QuickCheck based testing suite, the function's inability to find a counter-example does not suggest that 
one does not exist! Below, we have a function that runs 100 tests, which is usually sufficient to find a counter-example. 

\begin{code}
module Main where
import Syntax
import Semantics
import Parser

import Test.QuickCheck

\end{code}

After importing the necessary modules, we describe two small helper functions. 
The first is simply for notational convenience: we use it to check whether a given model falsifies a given formula. 

The second uses QuickCheck to generate arbitrary models, and terminates when it finds a model falsifying the given formula. 
The parameter \verb|maxSuccess| may be adjusted by the user to run as many tests as they require. 

\begin{code}
falsifies :: TeamPointedModel -> Form -> Bool
falsifies (TPM m s) f = not ((m, s) |= f)

findCounterexample :: Form -> IO ()
findCounterexample f = quickCheckWith stdArgs { maxSuccess = 100 } (`falsifies` f)

\end{code}

The \verb|main| function asks the user to provide a BSML formula as a string. 
It first checks that the string represents a well-formed formula using the parser, and then 
uses the helper functions above to produce (if it can find such an example, of course!) a model and a team that falsify
the formula provided by the user. 

\begin{code}
main :: IO ()
main = do
    putStrLn "Enter a BSML formula:"
    input <- getLine
    case parseFormula input of
        Left _ -> putStrLn "Sorry, that does not seem to be a well-formed formula."
        Right f -> findCounterexample f
\end{code}
