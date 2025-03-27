> {
> module Parser where
> import Data.Char
> import Token
> import Lexer
> import Defs
> }

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
> NUM { TokenInt $$ _ }
> '(' { TokenOB _ }
> ')' { TokenCB _}
>
> %left OR
> %left AND
>
> %%
>
> Form :: { Form }
> Form : BrForm { $1 }
> | Form AND Form { And $1 $3 }
> | Form OR Form { Or $1 $3 }
> BrForm :: { Form }
> BrForm : NUM { Prop $1 }
> | BOT { Bot }
> | NE { NE }
> | NOT BrForm { Neg $2 }
> | BOX BrForm { Defs.box $2 }
> | DMD BrForm { Dia $2 }
> | '(' Form AND Form ')' { And $2 $4 }
> | '(' Form OR Form ')' { Or $2 $4 }
>
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

> parseFormula :: String -> ParseResult Form
> parseFormula str = go $ myAlexScan str  >>= formula
>   where
>       go ( Left err ) = Left $ err { pe_str = str }
>       go ( Right res ) = Right res
>
> }


