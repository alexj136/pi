{
module Lexer where

import Util

import Control.Monad.State
import qualified Data.Map as M
}

%wrapper "basic"

tokens :-
    $white+ ;
    "@"     { \s -> TkAt      }
    "."     { \s -> TkDot     }
    "|"     { \s -> TkBar     }
    "/"     { \s -> TkFSl     }
    "!"     { \s -> TkBang    }
    "("     { \s -> TkLPar    }
    ")"     { \s -> TkRPar    }
    "0"     { \s -> TkZero    }
    [a-z]+  { \s -> TkSName s }

{
convertNames :: [Token] -> State (Name, M.Map String Name) [Token]
convertNames = sequence . map (\tk -> case tk of
    TkSName s -> freshFor s >>= return . TkName
    _         -> return tk)

data Token
    = TkAt
    | TkDot
    | TkBar
    | TkFSl
    | TkBang
    | TkLPar
    | TkRPar
    | TkZero
    | TkSName String
    | TkName Name
    deriving (Show, Eq, Ord)
}
