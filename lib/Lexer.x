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


