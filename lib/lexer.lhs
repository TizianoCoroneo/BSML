\section{Parsing/Lexing}

Below we describe our code for lexing and parsing BSML formulas.
We use the well-known tools Alex and Happy for lexing and parsing, respectively. 
This code is partially inspired by \cite{Katejan}. 


we start by telling Alex how to recognize certain (series of) symbols in the string we will be parsing and turn them into the corresponding token. 
These tokens are symbols that are assigned some meaning while a string is being parsed. 
Our set of tokens are defined in a separate file, \verb|Token.hs|, which is omitted here since it is utterly uninspiring. 
\begin{lstlisting}[mathescape = False]
{
module Lexer where
import Token
}


%wrapper "posn"
$dig = 0 -9 -- digits
tokens:-
-- ignore whitespace and comments :
$white + ;
" --" .* ;
-- keywords and punctuation :
"("   { \ p _ -> TokenOB p }
")"   { \ p _ -> TokenCB p }
"_|_" { \ p _ -> TokenBot p }
"NE"  { \ p _ -> TokenNE p}
"[]"  { \ p _ -> TokenBox p }
"<>"  { \ p _ -> TokenDmd p }
"~"   { \ p _ -> TokenNot p }
"&"   { \ p _ -> TokenCon p }
"v"   { \ p _ -> TokenDis p }
-- Integers and Strings :
$dig +  { \ p s -> TokenInt ( read s ) p }
[P - Z] { \ p s -> TokenPrp ( ord ( head s ) - ord 'P') p }
[p - z] { \ p s -> TokenPrp ( ord ( head s ) - ord 'p' + 11) p }
\end{lstlisting}

This file, \verb|Lexer.x| is \emph{not} a valid Haskell file, it is only meant to be input to Alex. 
A user may run \verb|alex Lexer.x| to generate \verb|Lexer.hs|, a fully functioning lexer for the above tokens built by Alex.