\section{An Executable Function}\label{sec:Main}

We now describe a very simple executable function. Upon writing \verb|main| in ghci, the function asks the user to provide a BSML formula as input. 
Using the parser, we parse this string, and try to come up with an example that does not satisfy the formula. 

Note that as with any other QuickCheck based testing suite, the function's inability to find a counter-example does not suggest that 
one does not exist! Below, we have a function that runs 100 tests (as standard, as we will soon see), which is usually sufficient to find a counter-example. 

\begin{code}
module Main where
import Syntax
import Semantics
import Parser

import Test.QuickCheck
import Text.Read (readMaybe)

\end{code}

After importing the necessary modules, we describe two small helper functions. 
The first is simply for notational convenience: we use it to check whether a given model falsifies a given formula. 

The second uses QuickCheck to generate arbitrary models, and terminates when it finds a model falsifying the given formula.  

\begin{code}
falsifies :: TeamPointedModel -> Form -> Bool
falsifies (TPM m s) f = not ((m, s) |= f)

findCounterexample :: Form -> Int -> IO ()
findCounterexample f n = quickCheck (withMaxSuccess n (`falsifies` f))

\end{code}

The \verb|main| function asks the user to provide a BSML formula as a string. 
It first checks that the string represents a well-formed formula using the parser, and then 
uses the helper functions above to produce (if it can find such an example, of course!) a model and a team that falsify
the formula provided by the user. The user is also asked to specify the maximum number of tests they wish to run within the function. The 
default number of tests is fixed at 100. 

\begin{code}
main :: IO ()
main = do
    putStrLn "Enter a BSML formula:"
    input <- getLine
    case parseFormula input of
        Left s -> putStrLn(pe_str s ++ pe_msg s ++ show(pe_col s))
        Right f -> do 
            putStrLn "Type in the maximum number of tests you wish to run (a non-number will result in 100 tests being run):"
            testnum <- getLine
            case (readMaybe testnum :: Maybe Int) of
                Nothing -> findCounterexample f 100
                Just x -> findCounterexample f x
\end{code}
