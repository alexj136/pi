module Main where

import Util
import Lexer
import Parser
import Syntax
import Interpreter

import Data.List (intersperse)
import System.Environment (getArgs)
import Control.Monad.State
import qualified Data.Map as M

main :: IO ()
main = do
    args <- getArgs
    let tks = alexScanTokens $ concat $ intersperse " " args
    let (tks', (n, names)) = runState (convertNames tks) (Name 0, M.empty)
    let pi = parse tks'
    let (pi', (n', names')) = runState (interpret pi) (n, swap names)
    putStrLn $ pp names' pi'
    return ()
