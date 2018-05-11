module Interpreter where

import Util
import Syntax

import Control.Monad.State
import qualified Data.Map as M

interpret :: PiTerm -> FreshName PiTerm
interpret p = do
    (n, names) <- get
    let (p', n', names') = fromInterpreterState $ snd $
            runState exec (InterpreterState [p] M.empty [] n names)
    put (n', names')
    return p'
    
fromInterpreterState :: InterpreterState -> (PiTerm, Name, M.Map Name String)
fromInterpreterState (InterpreterState run wait news n names) =
    (foldr New (foldl Par O (run ++ concat (map toPiTerms (M.assocs wait))))
        news, n, names)

data WaitQ
    = Senders [(Name, PiTerm)]
    | Receivers [(Name, PiTerm)]
    deriving (Show, Eq, Ord)

toPiTerms :: (Name, WaitQ) -> [PiTerm]
toPiTerms (n, (Senders   ss)) = map (\(x, p) -> Out n x p) ss
toPiTerms (n, (Receivers rs)) = map (\(x, p) -> In  n x p) rs

data InterpreterState =
    InterpreterState [PiTerm] (M.Map Name WaitQ) [Name] Name (M.Map Name String)
    deriving (Show, Eq, Ord)

type Interpreter = State InterpreterState

freshOp :: FreshName a -> Interpreter a
freshOp fna = do
    InterpreterState run wait news n names <- get
    let (a, (n', names')) = runState fna (n, names)
    put $ InterpreterState run wait news n' names'
    return a

putRun :: [PiTerm] -> Interpreter ()
putRun run = modify $ \(InterpreterState _ wait news n names) ->
    InterpreterState run wait news n names

getRun :: Interpreter [PiTerm]
getRun = gets $ \(InterpreterState run _ _ _ _) -> run

getWait :: Name -> Interpreter (Maybe WaitQ)
getWait x = gets $ \(InterpreterState _ wait _ _ _) -> M.lookup x wait

putWait :: Name -> WaitQ -> Interpreter ()
putWait x wq = modify $ \(InterpreterState run wait news n names) ->
    InterpreterState run (M.insert x wq wait) news n names

putNews :: Name -> Interpreter ()
putNews x = modify $ \(InterpreterState run wait news n names) ->
    InterpreterState run wait (x : news) n names

exec :: Interpreter ()
exec = getRun >>= \run -> case run of
    [] -> return ()
    Out x y p : ps -> putRun ps >> dispatchOut x y p >> exec
    In x y p : ps -> putRun ps >> dispatchIn x y p >> exec
    New x p : ps -> putRun ps >> dispatchNew x p >> exec
    Rep p : ps -> undefined
    Par p q : ps -> putRun ([p] ++ ps ++ [q]) >> exec
    O : ps -> putRun ps >> exec

dispatchOut :: Name -> Name -> PiTerm -> Interpreter ()
dispatchOut x y p = getWait x >>= \wq -> case wq of
    Just (Receivers ((z, q):rs)) -> do
        run <- getRun
        q' <- freshOp $ (y // z) q
        putRun $ [p] ++ run ++ [q']
        putWait x (Receivers rs)
    Just (Receivers []) -> putWait x (Senders [(y, p)])
    Just (Senders ss) -> putWait x (Senders (ss ++ [(y, p)]))
    Nothing -> putWait x (Senders [(y, p)])

dispatchIn :: Name -> Name -> PiTerm -> Interpreter ()
dispatchIn x y p = getWait x >>= \wq -> case wq of
    Just (Senders ((z, q):ss)) -> do
        run <- getRun
        p' <- freshOp $ (z // y) p
        putRun $ [p'] ++ run ++ [q]
        putWait x (Senders ss)
    Just (Senders []) -> putWait x (Receivers [(y, p)])
    Just (Receivers rs) -> putWait x (Receivers (rs ++ [(y, p)]))
    Nothing -> putWait x (Receivers [(y, p)])

dispatchNew :: Name -> PiTerm -> Interpreter ()
dispatchNew n p = do
    x <- freshOp fresh
    p' <- freshOp $ (x // n) p
    run <- getRun
    putRun $ p' : run
    putNews x
