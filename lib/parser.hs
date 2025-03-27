{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UnboxedTuples #-}


{-# LANGUAGE PartialTypeSignatures #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use lambda-case" #-}

module Parser where
import Data.Char
import Token
import Lexer
import Defs
import qualified Data.Function as Happy_Prelude
import qualified Data.Bool as Happy_Prelude
import qualified Data.Maybe as Happy_Prelude
import qualified Data.Int as Happy_Prelude
import qualified Data.String as Happy_Prelude
import qualified Data.Tuple as Happy_Prelude
import qualified Data.List as Happy_Prelude
import qualified Control.Monad as Happy_Prelude
import qualified Text.Show as Happy_Prelude
import qualified GHC.Num as Happy_Prelude
import qualified GHC.Err as Happy_Prelude
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.1.5

data HappyAbsSyn
        = HappyTerminal (Token AlexPosn)
        | HappyErrorToken Happy_Prelude.Int
        | HappyAbsSyn5 Form

{-# NOINLINE happyTokenStrings #-}
happyTokenStrings = ["BOT","NE","AND","OR","NOT","BOX","DMD","NUM","'('","')'","%eof"]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x00\x00\xff\xff\xff\xff\x06\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x03\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xff\xff\xfa\xff\xff\xff\xf9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\xff\xff\xff\xf7\xff\xff\xff\xf8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\xff\xff\xfd\xff\xff\xff\xfc\xff\xff\xff\xfd\xff\xff\xff\xf5\xff\xff\xff\xf4\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x02\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x04\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x04\x00\x00\x00\x0c\x00\x00\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x0a\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0c\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x0d\x00\x00\x00\x02\x00\x00\x00\x0c\x00\x00\x00\xff\xff\xff\xff\x19\x00\x00\x00\x14\x00\x00\x00\x02\x00\x00\x00\x13\x00\x00\x00\x02\x00\x00\x00\x16\x00\x00\x00\x02\x00\x00\x00\x15\x00\x00\x00\x02\x00\x00\x00\x18\x00\x00\x00\x02\x00\x00\x00\x10\x00\x00\x00\x0f\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 11) [
        (1 , happyReduce_1),
        (2 , happyReduce_2),
        (3 , happyReduce_3),
        (4 , happyReduce_4),
        (5 , happyReduce_5),
        (6 , happyReduce_6),
        (7 , happyReduce_7),
        (8 , happyReduce_8),
        (9 , happyReduce_9),
        (10 , happyReduce_10),
        (11 , happyReduce_11)
        ]

happyRuleArr :: HappyAddr
happyRuleArr = HappyA# "\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x01\x00\x00\x00\x05\x00\x00\x00\x01\x00\x00\x00\x05\x00\x00\x00"#

happyCatchStates :: [Happy_Prelude.Int]
happyCatchStates = []

happy_n_terms = 13 :: Happy_Prelude.Int
happy_n_nonterms = 2 :: Happy_Prelude.Int

happy_n_starts = 1 :: Happy_Prelude.Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 happy_var_1
happyReduction_1 _  = notHappyAtAll

happyReduce_2 = happySpecReduce_3  0# happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
        _
        (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (And happy_var_1 happy_var_3
        )
happyReduction_2 _ _ _  = notHappyAtAll

happyReduce_3 = happySpecReduce_3  0# happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_3)
        _
        (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (Or happy_var_1 happy_var_3
        )
happyReduction_3 _ _ _  = notHappyAtAll

happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 (HappyTerminal (TokenInt happy_var_1 _))
         =  HappyAbsSyn5
                 (Prop happy_var_1
        )
happyReduction_4 _  = notHappyAtAll

happyReduce_5 = happySpecReduce_1  1# happyReduction_5
happyReduction_5 _
         =  HappyAbsSyn5
                 Bot

happyReduce_6 = happySpecReduce_1  1# happyReduction_6
happyReduction_6 _
         =  HappyAbsSyn5
                 NE

happyReduce_7 = happySpecReduce_2  1# happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_2)
        _
         =  HappyAbsSyn5
                 (Neg happy_var_2
        )
happyReduction_7 _ _  = notHappyAtAll

happyReduce_8 = happySpecReduce_2  1# happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_2)
        _
         =  HappyAbsSyn5
                 (Defs.box happy_var_2
        )
happyReduction_8 _ _  = notHappyAtAll

happyReduce_9 = happySpecReduce_2  1# happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_2)
        _
         =  HappyAbsSyn5
                 (Dia happy_var_2
        )
happyReduction_9 _ _  = notHappyAtAll

happyReduce_10 = happyReduce 5# 1# happyReduction_10
happyReduction_10 (_ `HappyStk`
        (HappyAbsSyn5  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn5  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn5
                 (And happy_var_2 happy_var_4
        ) `HappyStk` happyRest

happyReduce_11 = happyReduce 5# 1# happyReduction_11
happyReduction_11 (_ `HappyStk`
        (HappyAbsSyn5  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn5  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn5
                 (Or happy_var_2 happy_var_4
        ) `HappyStk` happyRest

happyTerminalToTok term = case term of {
        TokenBot _ -> 2#;
        TokenNE _ -> 3#;
        TokenCon _ -> 4#;
        TokenDis _ -> 5#;
        TokenNot _ -> 6#;
        TokenBox _ -> 7#;
        TokenDmd _ -> 8#;
        TokenInt happy_dollar_dollar _ -> 9#;
        TokenOB _ -> 10#;
        TokenCB _ -> 11#;
        _ -> -1#;
        }
{-# NOINLINE happyTerminalToTok #-}

happyLex kend  _kmore []       = kend notHappyAtAll []
happyLex _kend kmore  (tk:tks) = kmore (happyTerminalToTok tk) tk tks
{-# INLINE happyLex #-}

happyNewToken action sts stk = happyLex (\tk -> happyDoAction 12# notHappyAtAll action sts stk) (\i tk -> happyDoAction i tk action sts stk)

happyReport 12# tk explist resume tks = happyReport' tks explist resume
happyReport _ tk explist resume tks = happyReport' (tk:tks) explist (resume . Happy_Prelude.tail)


happyThen :: () => ParseResult a -> (a -> ParseResult b) -> ParseResult b
happyThen = (Happy_Prelude.>>=)
happyReturn :: () => a -> ParseResult a
happyReturn = Happy_Prelude.return
happyThen1 m k tks = (Happy_Prelude.>>=) m (`k` tks)
happyFmap1 f m tks = happyThen (m tks) (happyReturn . f)
happyReturn1 :: () => a -> b -> ParseResult a
happyReturn1 a tks = Happy_Prelude.return a
happyReport' :: () => [Token AlexPosn] -> [Happy_Prelude.String] -> ([Token AlexPosn] -> ParseResult a) -> ParseResult a
happyReport' tokens expected resume = happyError tokens

happyAbort :: () => [Token AlexPosn] -> ParseResult a
happyAbort = Happy_Prelude.error "Called abort handler in non-resumptive parser"

formula tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data ParseError = ParseError { pe_str :: String
                              ,pe_msg :: String
                              ,pe_col :: Int}
     deriving (Eq , Show)
type ParseResult a = Either ParseError a
happyError :: [ Token AlexPosn ] -> ParseResult a
happyError [] = Left $
     ParseError { pe_str = " " , pe_msg = " Unexpected end of input : " , pe_col = -1}
happyError ( t : ts ) = Left $
     ParseError { pe_str = " " , pe_msg = " Parse error : " , pe_col = col }
     where ( AlexPn abs lin col ) = apn t
myAlexScan :: String -> ParseResult [ Token AlexPosn ]
myAlexScan str = go ( alexStartPos , '\n' ,[] , str )
  where
      go :: AlexInput -> ParseResult [ Token AlexPosn ]
      go inp@( pos ,_ ,_ , str ) =
          case alexScan inp 0 of
              AlexEOF -> Right []
              AlexError (AlexPn _ _ column ,_ ,_ , _ ) -> Left $
                  ParseError { pe_str = str , pe_msg = " Lexical error : " , pe_col = column -1}
              AlexSkip inp' len -> go inp'
              AlexToken inp' len act -> go inp'  >=
                  (\ x -> Right $ act pos ( take len str ) : x )

parseFormula :: String -> ParseResult Form
parseFormula str = go $ myAlexScan str  >= formula
  where
      go ( Left err ) = Left $ err { pe_str = str }
      go ( Right res ) = Right res
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $





-- Get WORDS_BIGENDIAN (if defined)


-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.








type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList
























infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        happyTcHack j (happyTcHack st) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  {- nothing -}


  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> {- nothing -}
                             happyFail i tk st
    HappyAccept           -> {- nothing -}
                             happyAccept i tk st
    HappyReduce rule      -> {- nothing -}
                             (happyReduceArr Happy_Data_Array.! Happy_GHC_Exts.I# rule) i tk st
    HappyShift  new_state -> {- nothing -}
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Happy_Prelude.Just (Happy_GHC_Exts.I# act) -> act
  Happy_Prelude.Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | Happy_GHC_Exts.tagToEnum# (i Happy_GHC_Exts.>=#  0#) :: Happy_Prelude.Bool, Happy_GHC_Exts.tagToEnum# (off Happy_GHC_Exts.>=#  0#) :: Happy_Prelude.Bool, Happy_GHC_Exts.tagToEnum# (happyIndexOffAddr happyCheck off Happy_GHC_Exts.==#  i) :: Happy_Prelude.Bool
  -- i >= 0:   Guard against -1# (do the default action, which ultimately errors)
  -- off >= 0: Otherwise it's a default action
  -- equality check: Ensure that the entry in the compressed array is owned by st
  = Happy_Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | Happy_Prelude.otherwise
  = Happy_Prelude.Nothing
  where
    off = happyIndexOffAddr happyActOffsets st Happy_GHC_Exts.+#  i

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state
  deriving Happy_Prelude.Show

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | Happy_GHC_Exts.tagToEnum# (action Happy_GHC_Exts.<#  0#) :: Happy_Prelude.Bool    = HappyReduce (Happy_GHC_Exts.negateInt# (action Happy_GHC_Exts.+#  1#))
                         | Happy_Prelude.otherwise = HappyShift (action Happy_GHC_Exts.-#  1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = happyIndexOffAddr happyGotoOffsets st Happy_GHC_Exts.+#  nt

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =

  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's





  (Happy_GHC_Exts.indexInt32OffAddr# arr off)




happyIndexRuleArr :: Happy_Int -> (# Happy_Int, Happy_Int #)
happyIndexRuleArr r = (# nt, len #)
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts
    offs = (r Happy_GHC_Exts.-# n_starts) Happy_GHC_Exts.*# 2#
    nt = happyIndexOffAddr happyRuleArr offs
    len = happyIndexOffAddr happyRuleArr (offs Happy_GHC_Exts.+# 1#)

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     -- See "Error Fixup" below
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i }) in
     {- nothing -}
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (HappyTerminal tk `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 nt fn j tk st sts stk
     = happySeq fn (happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk))

happySpecReduce_1 nt fn j tk old_st sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_2 nt fn j tk old_st
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_3 nt fn j tk old_st
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                st `happyTcHack` happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          j `happyTcHack` happyThen1 (fn stk tk)
                                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyIndexOffAddr happyGotoOffsets st1
              off_i = (off Happy_GHC_Exts.+#  nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            j `happyTcHack` happyThen1 (fn stk tk)
                                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   {- nothing -}
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

{- Note [Error recovery]
~~~~~~~~~~~~~~~~~~~~~~~~
When there is no applicable action for the current lookahead token `tk`,
happy enters error recovery mode. Depending on whether the grammar file
declares the two action form `%error { abort } { report }` for
    Resumptive Error Handling,
it works in one (not resumptive) or two phases (resumptive):

 1. Fixup mode:
    Try to see if there is an action for the error token 0#. If there
    is, do *not* emit an error and pretend instead that an `error` token was
    inserted.
    When there is no 0# action, report an error.

    In non-resumptive error handling, calling the single error handler
    (e.g. `happyError`) will throw an exception and abort the parser.
    However, in resumptive error handling we enter *error resumption mode*.

 2. Error resumption mode:
    After reporting the error (with `report`), happy will attempt to find
    a good state stack to resume parsing in.
    For each candidate stack, it discards input until one of the candidates
    resumes (i.e. shifts the current input).
    If no candidate resumes before the end of input, resumption failed and
    calls the `abort` function, to much the same effect as in non-resumptive
    error handling.

    Candidate stacks are declared by the grammar author using the special
    `catch` terminal and called "catch frames".
    This mechanism is described in detail in Note [happyResume].

The `catch` resumption mechanism (2) is what usually is associated with
`error` in `bison` or `menhir`. Since `error` is used for the Fixup mechanism
(1) above, we call the corresponding token `catch`.
Furthermore, in constrast to `bison`, our implementation of `catch`
non-deterministically considers multiple catch frames on the stack for
resumption (See Note [Multiple catch frames]).

Note [happyResume]
~~~~~~~~~~~~~~~~~~
`happyResume` implements the resumption mechanism from Note [Error recovery].
It is best understood by example. Consider

Exp :: { String }
Exp : '1'                { "1" }
    | catch              { "catch" }
    | Exp '+' Exp %shift { $1 Happy_Prelude.++ " + " Happy_Prelude.++ $3 } -- %shift: associate 1 + 1 + 1 to the right
    | '(' Exp ')'        { "(" Happy_Prelude.++ $2 Happy_Prelude.++ ")" }

The idea of the use of `catch` here is that upon encountering a parse error
during expression parsing, we can gracefully degrade using the `catch` rule,
still producing a partial syntax tree and keep on parsing to find further
syntax errors.

Let's trace the parser state for input 11+1, which will error out after shifting 1.
After shifting, we have the following item stack (growing downwards and omitting
transitive closure items):

  State 0: %start_parseExp -> . Exp
  State 5: Exp -> '1' .

(Stack as a list of state numbers: [5,0].)
As Note [Error recovery] describes, we will first try Fixup mode.
That fails because no production can shift the `error` token.
Next we try Error resumption mode. This works as follows:

  1. Pop off the item stack until we find an item that can shift the `catch`
     token. (Implemented in `pop_items`.)
       * State 5 cannot shift catch. Pop.
       * State 0 can shift catch, which would transition into
          State 4: Exp -> catch .
     So record the *stack* `[4,0]` after doing the shift transition.
     We call this a *catch frame*, where the top is a *catch state*,
     corresponding to an item in which we just shifted a `catch` token.
     There can be multiple such catch stacks, see Note [Multiple catch frames].

  2. Discard tokens from the input until the lookahead can be shifted in one
     of the catch stacks. (Implemented in `discard_input_until_exp` and
     `some_catch_state_shifts`.)
       * We cannot shift the current lookahead '1' in state 4, so we discard
       * We *can* shift the next lookahead '+' in state 4, but only after
         reducing, which pops State 4 and goes to State 3:
           State 3: %start_parseExp -> Exp .
                    Exp -> Exp . '+' Exp
         Here we can shift '+'.
     As you can see, to implement this machinery we need to simulate
     the operation of the LALR automaton, especially reduction
     (`happySimulateReduce`).

Note [Multiple catch frames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For fewer spurious error messages, it can be beneficial to trace multiple catch
items. Consider

Exp : '1'
    | catch
    | Exp '+' Exp %shift
    | '(' Exp ')'

Let's trace the parser state for input (;+1, which will error out after shifting (.
After shifting, we have the following item stack (growing downwards):

  State 0: %start_parseExp -> . Exp
  State 6: Exp -> '(' . Exp ')'

Upon error, we want to find items in the stack which can shift a catch token.
Note that both State 0 and State 6 can shift a catch token, transitioning into
  State 4: Exp -> catch .
Hence we record the catch frames `[4,6,0]` and `[4,0]` for possible resumption.

Which catch frame do we pick for resumption?
Note that resuming catch frame `[4,0]` will parse as "catch+1", whereas
resuming the innermost frame `[4,6,0]` corresponds to parsing "(catch+1".
The latter would keep discarding input until the closing ')' is found.
So we will discard + and 1, leading to a spurious syntax error at the end of
input, aborting the parse and never producing a partial syntax tree. Bad!

It is far preferable to resume with catch frame `[4,0]`, where we can resume
successfully on input +, so that is what we do.

In general, we pick the catch frame for resumption that discards the least
amount of input for a successful shift, preferring the topmost such catch frame.
-}

-- happyFail :: Happy_Int -> Token -> Happy_Int -> _
-- This function triggers Note [Error recovery].
-- If the current token is 0#, phase (1) has failed and we might try
-- phase (2).
happyFail 0# = happyFixupFailed
happyFail i         = happyTryFixup i

-- Enter Error Fixup (see Note [Error recovery]):
-- generate an error token, save the old token and carry on.
-- When a `happyShift` accepts the error token, we will pop off the error token
-- to resume parsing with the current lookahead `i`.
happyTryFixup i tk action sts stk =
  {- nothing -}
  happyDoAction 0# tk action sts (HappyErrorToken (Happy_GHC_Exts.I# i) `HappyStk` stk)
  -- NB: `happyShift` will simply pop the error token and carry on with
  --     `tk`. Hence we don't change `tk` in the call here

-- See Note [Error recovery], phase (2).
-- Enter resumption mode after reporting the error by calling `happyResume`.
happyFixupFailed tk st sts (x `HappyStk` stk) =
  let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i }) in
  {- nothing -}
  let resume   = happyResume i tk st sts stk
      expected = happyExpectedTokens st sts in
  happyReport i tk expected resume

-- happyResume :: Happy_Int -> Token -> Happy_Int -> _
-- See Note [happyResume]
happyResume i tk st sts stk = pop_items [] st sts stk
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts   -- this is to test whether we have a start token
    !(Happy_GHC_Exts.I# eof_i) = happy_n_terms Happy_Prelude.- 1   -- this is the token number of the EOF token
    happy_list_to_list :: Happy_IntList -> [Happy_Prelude.Int]
    happy_list_to_list (HappyCons st sts)
      | Happy_GHC_Exts.tagToEnum# (st Happy_GHC_Exts.<#  n_starts) :: Happy_Prelude.Bool
      = [Happy_GHC_Exts.I# st]
      | Happy_Prelude.otherwise
      = Happy_GHC_Exts.I# st : happy_list_to_list sts

    -- See (1) of Note [happyResume]
    pop_items catch_frames st sts stk
      | Happy_GHC_Exts.tagToEnum# (st Happy_GHC_Exts.<#  n_starts) :: Happy_Prelude.Bool
      = {- nothing -}
        if Happy_Prelude.null catch_frames_new
          then {- nothing -}
               happyAbort
          else {- nothing -}
               discard_input_until_exp i tk (Happy_Prelude.reverse catch_frames_new)
      | (HappyCons st1 sts1) <- sts, _ `HappyStk` stk1 <- stk
      = pop_items catch_frames_new st1 sts1 stk1
      where
        !catch_frames_new
          | HappyShift new_state <- happyDecodeAction (happyNextAction 1# st)
          , {- nothing -}
            not (any (\(HappyCons _ (HappyCons h _),_) -> (Happy_GHC_Exts.tagToEnum# (st Happy_GHC_Exts.==# h) :: Happy_Prelude.Bool)) catch_frames)
          = (HappyCons new_state (HappyCons st sts), HappyErrorToken (Happy_GHC_Exts.I# i) `HappyStk` stk):catch_frames -- (HappyErrorToken (Happy_GHC_Exts.I# i)) is just some dummy that should not be accessed by user code
          | Happy_Prelude.otherwise
          = {- nothing -}
            catch_frames

    -- See (2) of Note [happyResume]
    discard_input_until_exp i tk catch_frames
      | Happy_Prelude.Just (HappyCons st (HappyCons catch_st sts), catch_frame) <- some_catch_state_shifts i catch_frames
      = {- nothing -}
        happyDoAction i tk st (HappyCons catch_st sts) catch_frame
      | Happy_GHC_Exts.tagToEnum# (i Happy_GHC_Exts.==# eof_i) :: Happy_Prelude.Bool -- is i EOF?
      = {- nothing -}
        happyAbort
      | Happy_Prelude.otherwise
      = {- nothing -}
        happyLex (\eof_tk -> discard_input_until_exp eof_i eof_tk catch_frames) -- eof
                 (\i tk   -> discard_input_until_exp i tk catch_frames)         -- not eof

    some_catch_state_shifts _ [] = {- nothing -} Happy_Prelude.Nothing
    some_catch_state_shifts i catch_frames@((HappyCons st sts,_):_) = try_head i st sts catch_frames
      where
        try_head i st sts catch_frames = -- PRECONDITION: head catch_frames = (HappyCons st sts)
          {- nothing -}
          case happyDecodeAction (happyNextAction i st) of
            HappyFail     -> {- nothing -}   some_catch_state_shifts i (Happy_Prelude.tail catch_frames)
            HappyAccept   -> {- nothing -} Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyShift _  -> {- nothing -}  Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyReduce r -> case happySimulateReduce r st sts of
              (HappyCons st1 sts1) -> try_head i st1 sts1 catch_frames

happySimulateReduce r st sts =
  {- nothing -}
  let (# nt, len #) = happyIndexRuleArr r in
  {- nothing -}
  let !sts1@(HappyCons st1 _) = happyDrop len (HappyCons st sts)
      new_st = happyIndexGotoTable nt st1 in
  {- nothing -}
  HappyCons new_st sts1

happyTokenToString :: Happy_Prelude.Int -> Happy_Prelude.String
happyTokenToString i = happyTokenStrings Happy_Prelude.!! (i Happy_Prelude.- 2) -- 2: errorTok, catchTok

happyExpectedTokens :: Happy_Int -> Happy_IntList -> [Happy_Prelude.String]
-- Upon a parse error, we want to suggest tokens that are expected in that
-- situation. This function computes such tokens.
-- It works by examining the top of the state stack.
-- For every token number that does a shift transition, record that token number.
-- For every token number that does a reduce transition, simulate that reduction
-- on the state state stack and repeat.
-- The recorded token numbers are then formatted with 'happyTokenToString' and
-- returned.
happyExpectedTokens st sts =
  {- nothing -}
  Happy_Prelude.map happyTokenToString (search_shifts st sts [])
  where
    search_shifts st sts shifts = Happy_Prelude.foldr (add_action st sts) shifts (distinct_actions st)
    add_action st sts (Happy_GHC_Exts.I# i, Happy_GHC_Exts.I# act) shifts =
      {- nothing -}
      case happyDecodeAction act of
        HappyFail     -> shifts
        HappyAccept   -> shifts -- This would always be %eof or error... Not helpful
        HappyShift _  -> Happy_Prelude.insert (Happy_GHC_Exts.I# i) shifts
        HappyReduce r -> case happySimulateReduce r st sts of
          (HappyCons st1 sts1) -> search_shifts st1 sts1 shifts
    distinct_actions st
      -- The (token number, action) pairs of all actions in the given state
      = (-1, Happy_GHC_Exts.I# (happyIndexOffAddr happyDefActions st))
      : [ (i, act) | i <- [begin_i..happy_n_terms], act <- get_act row_off i ]
      where
        row_off = happyIndexOffAddr happyActOffsets st
        begin_i = 2 -- +2: errorTok,catchTok
    get_act off (Happy_GHC_Exts.I# i) -- happyIndexActionTable with cached row offset
      | let off_i = off Happy_GHC_Exts.+# i
      , Happy_GHC_Exts.tagToEnum# (off_i Happy_GHC_Exts.>=# 0#) :: Happy_Prelude.Bool
      , Happy_GHC_Exts.tagToEnum# (happyIndexOffAddr happyCheck off_i Happy_GHC_Exts.==# i) :: Happy_Prelude.Bool
      = [Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off_i)]
      | Happy_Prelude.otherwise
      = []

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Happy_Prelude.error "Internal Happy parser panic. This is not supposed to happen! Please open a bug report at https://github.com/haskell/happy/issues.\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Happy_GHC_Exts.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.




