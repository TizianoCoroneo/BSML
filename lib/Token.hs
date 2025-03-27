module Token where
data Token a
    = TokenPrp { fooP :: Int , apn :: a }
    | TokenNE  { apn :: a}
    | TokenBot { apn :: a }
    | TokenCon { apn :: a }
    | TokenDis { apn :: a }
    | TokenNot { apn :: a }
    | TokenBox { apn :: a }
    | TokenDmd { apn :: a }
    | TokenInt { fooS :: Int , apn :: a }
    | TokenOB { apn :: a }
    | TokenCB { apn :: a }
    deriving Show

