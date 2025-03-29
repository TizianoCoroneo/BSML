
We now take a look at the code for the parser, which works using Happy.
We first import the modules, including \verb|Lexer|, that we require to run the parser.

\begin{code}
> {
> module Parser where
>
> import Data.Char
> import Token
> import Lexer
> import Syntax
> import Semantics
> import ML
> }

\end{code}

Below, we describe the tokens that Happy needs to keep track of while
reading the string. These tokens are congruent with those in the module Lexer.

\begin{code}

> %name formula
> %tokentype { Token AlexPosn }
> %monad { ParseResult }
>
> %token
> BOT { TokenBot _ }
> NE {TokenNE _}
> AND { TokenCon _ }
> OR { TokenDis _ }
> NOT { TokenNot _ }
> BOX { TokenBox _ }
> DMD { TokenDmd _ }
> GDIS {TokenGDis _}
> NUM { TokenInt $$ _ }
> '(' { TokenOB _ }
> ')' { TokenCB _}
\end{code}

We describe also the binding hierarchy for binary operators in our language.
The order of precedence is described by listing operators from weakest to strongest,
as evidenced below. Note that the binding for all unary operators is stronger than the
binding for binary operators, and unary operators operate at the same binding strength.
This behaviour keeps in line with the way we would like to parse formulas in our language.

\begin{code}
> %left OR
> %left AND
> %left GDIS
>
> %%
\end{code}

We now detail two different types of formulas - bracketed formulas and
non-bracketed formulas. The reasoning for the distinction is rather simple -
the presence of parentheses around a formula requires that any operations within the
parentheses need to be given priority over operations outside of them. This may break
regular precedence rules, and hence need to be accounted for.

\begin{code}
> Form :: { Form }
> Form : BrForm { $1 }
> | Form AND Form { And $1 $3 }
> | Form OR Form { Or $1 $3 }
> | Form GDIS Form {Gor $1 $3}
> BrForm :: { Form }
> BrForm : NUM { Prop $1 }
> | BOT { Bot }
> | NE { NE }
> | NOT BrForm { Neg $2 }
> | BOX BrForm { Syntax.box $2 }
> | DMD BrForm { Dia $2 }
> | '(' Form AND Form ')' { And $2 $4 }
> | '(' Form OR Form ')' { Or $2 $4 }
> | '(' Form GDIS Form ')' { Gor $2 $4}
\end{code}

Next, we define error messages for our parser, as in \cite{Katejan}.
These error messages describe where the error occurs exactly in the string, and why Happy failed
to parse it.

\begin{code}
> {
> data ParseError = ParseError { pe_str :: String
>                               ,pe_msg :: String
>                               ,pe_col :: Int}
>      deriving (Eq , Show)
>
> type ParseResult a = Either ParseError a
>
>
> happyError :: [ Token AlexPosn ] -> ParseResult a
> happyError [] = Left $
>      ParseError { pe_str = " " , pe_msg = " Unexpected end of input : " , pe_col = -1}
> happyError ( t : ts ) = Left $
>      ParseError { pe_str = " " , pe_msg = " Parse error : " , pe_col = col }
>      where ( AlexPn abs lin col ) = apn t
>
> myAlexScan :: String -> ParseResult [ Token AlexPosn ]
> myAlexScan str = go ( alexStartPos , '\n' ,[] , str )
>   where
>       go :: AlexInput -> ParseResult [ Token AlexPosn ]
>       go inp@( pos ,_ ,_ , str ) =
>           case alexScan inp 0 of
>               AlexEOF -> Right []
>               AlexError (( AlexPn _ _ column ) ,_ ,_ , _ ) -> Left $
>                   ParseError { pe_str = str , pe_msg = " Lexical error : " , pe_col = column -1}
>               AlexSkip inp' len -> go inp'
>               AlexToken inp' len act -> go inp'  >>=
>                   (\ x -> Right $ act pos ( take len str ) : x )
\end{code}

Finally, we describe the actual parsing function itself, called \texttt{parseFormula}. Upon
running \texttt{happy Parser.ly}, we get a Haskell file \texttt{Parser.hs} which contains
the parseFormula function. The output for parseFormula is of type \texttt{Either ParseError Form},
since parser might fail (on invalid input).

\begin{code}
> parseFormula :: String -> ParseResult Form
> parseFormula str = go $ myAlexScan str  >>= formula
>   where
>       go ( Left err ) = Left $ err { pe_str = str }
>       go ( Right res ) = Right res
>
> }
\end{code}

