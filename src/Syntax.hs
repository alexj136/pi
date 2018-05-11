module Syntax where

import Util
import Control.Monad (liftM, liftM2, liftM3)

data PiTerm
    = Par PiTerm PiTerm
    | New Name PiTerm
    | Out Name Name PiTerm
    | In Bool Name Name PiTerm
    | O
    deriving (Show, Eq, Ord)

instance PrettyPrint PiTerm where
    pp names pt = case pt of
        Par p q   -> "(" ++ pp names p ++ " | " ++ pp names q ++ ")"
        New x p   -> "(@" ++ pp names x ++ ")" ++ pp names p
        Out x y O -> pp names x ++ "/" ++ pp names y
        In r x y O  ->
            (if r then "!" else "") ++ pp names x ++ "(" ++ pp names y ++ ")"
        Out x y p -> pp names (Out x y O) ++ "." ++ pp names p
        In r x y p  -> pp names (In r x y O) ++ "." ++ pp names p
        O         -> "0"

instance Sub PiTerm where
    (//) x y proc = case proc of
        Out a b q -> liftM3 Out ((x // y) a) ((x // y) b) ((x // y) q)
        In r a b q | b == y ->
            (fresh >>= \z -> liftM (In r a z) ((z // y) q)) >>= (x // y)
        In r a b q | b /= y ->
            liftM3 (In r) ((x // y) a) (return b) ((x // y) q) 
        New a q | a == y ->
            (fresh >>= \z -> liftM (New z) ((z // y) q)) >>= (x // y)
        New a q | a /= y -> liftM (New a) ((x // y) q) 
        Par q r -> liftM2 Par ((x // y) q) ((x // y) r)
        O -> return O
