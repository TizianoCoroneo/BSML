\section{Lexer}

Below we describe a brief code for a lexer we built for BSML formulae. The lexer
was built to utilise the alex lexing tool mentioned in class.

We note that this code is heavily based on the code of Katejan Dvoracek,
who built a Natural Deduction Prover, and used within it a lexer running on
Alex, and a parser running on Happy. You can find the PDF for the project \href{https://canvas.uva.nl/courses/49699/assignments/559003?module_item_id=2279238}{here.}
Thanks a bunch Katejan!

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
"(" { \ p _ -> TokenOB p }
")" { \ p _ -> TokenCB p }
"_|_" { \ p _ -> TokenBot p }
"NE" {\ p _ -> TokenNE p}
"[]" { \ p _ -> TokenBox p }
"<>" { \ p _ -> TokenDmd p }
"~" { \ p _ -> TokenNot p }
"&" { \ p _ -> TokenCon p }
"v" { \ p _ -> TokenDis p }
-- Integers and Strings :
$dig + { \ p s -> TokenInt ( read s ) p }
[P - Z ] { \ p s -> TokenPrp ( ord ( head s ) - ord 'P') p }
[p - z ] { \ p s -> TokenPrp ( ord ( head s ) - ord 'p' + 11) p }
\end{lstlisting}

The basic overview of the file above is as follows: We instruct Alex to construct a
lexer that goes through a given string and picks out "tokens" from it. These tokens
are symbols to be kept in mind while the string is being assigned meaning - which is done using
a parser built by Happy.

This particular file has a .x extension - once written, the user may run alex Lexer.x in their terminal.
This command generates Lexer.hs, a fully functioning lexer for the above tokens built by Alex.
The code is housed inside a module called Lexer, which we import within the parser file.