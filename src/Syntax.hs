module Syntax where

import Util
import Control.Monad (liftM, liftM2, liftM3)

data PiTerm
    = Par PiTerm PiTerm
    | Rep PiTerm
    | New Name PiTerm
    | Out Name Name PiTerm
    | In Name Name PiTerm
    | O
    deriving (Show, Eq, Ord)

instance PrettyPrint PiTerm where
    pp names pt = case pt of
        Par p q   -> "(" ++ pp names p ++ " | " ++ pp names q ++ ")"
        Rep p     -> "!" ++ pp names p
        New x p   -> "(@" ++ pp names x ++ ")" ++ pp names p
        Out x y O -> pp names x ++ "/" ++ pp names y
        In x y O  -> pp names x ++ "(" ++ pp names y ++ ")"
        Out x y p -> pp names (Out x y O) ++ "." ++ pp names p
        In x y p  -> pp names (In x y O) ++ "." ++ pp names p
        O         -> "0"

instance Sub PiTerm where
    (//) x y proc = case proc of
        Out a b q -> liftM3 Out ((x // y) a) ((x // y) b) ((x // y) q)
        In a b q | b == y ->
            (fresh >>= \z -> liftM (In a z) ((z // y) q)) >>= (x // y)
        In a b q | b /= y -> liftM3 In ((x // y) a) (return b) ((x // y) q) 
        New a q | a == y ->
            (fresh >>= \z -> liftM (New z) ((z // y) q)) >>= (x // y)
        New a q | a /= y -> liftM (New a) ((x // y) q) 
        Rep q -> liftM Rep ((x // y) q)
        Par q r -> liftM2 Par ((x // y) q) ((x // y) r)
        O -> return O
