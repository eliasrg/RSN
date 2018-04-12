{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Interpreter where

import AST

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import Data.IORef
import Data.Maybe
import qualified Data.Map as M
import Text.Printf

data Value =
    Unit
  | Bool Bool
  | Int Integer
  | String String
  | Function Closure VariableName (Computation Value)
  | LazyValue Closure (Computation Value)

instance Show Value where
    show Unit = "UNIT"
    show (Bool b) = show b
    show (Int n) = show n
    show (String s) = show s
    show (Function _ _ _) = "<function>"
    show (LazyValue _ _) = "<lazy value>"

type Scope = M.Map VariableName Value
type VariableContext = [Scope]
data ProgramState = ProgramState {
    context :: VariableContext,
    closureDepth :: Maybe Int
    }
    deriving (Show)
type Computation a = StateT ProgramState IO a
type Closure = IORef (Scope, Int)


run :: Computation a -> IO a
run comp = let state =
                ProgramState { context = [M.empty], closureDepth = Nothing }
           in evalStateT comp state


interpret :: Expression -> Computation Value

interpret (Block e) = inNewScope (interpret e)
interpret (Chain es) = last <$> mapM interpret es
interpret (Lambda name e) = do
    closure <- mkClosure
    return (Function closure name (interpret e))
interpret (Lazy e) = do
    closure <- mkClosure
    return (LazyValue closure (interpret e))
interpret (LazyEvaluate e) = do
    f@(LazyValue closure comp) <- interpret e

    -- Add closure to variable context
    d <- closureDepth <$> get
    (closureScope, d') <- liftIO (readIORef closure)
    modifyContext (closureScope :)
    setClosureDepth (Just d')

    -- Call
    ret <- inNewScope $ do
        modifyContext (declareVar "rec" f)
        comp

    -- Update closure
    liftIO . writeIORef closure =<< (,d') . head . context <$> get

    -- Restore variable context (remove closure scope)
    modifyContext tail
    setClosureDepth d

    return ret
interpret (Call ex ef) = do
    x <- interpret ex
    f@(Function closure name comp) <- interpret ef

    -- Add closure to variable context
    d <- closureDepth <$> get
    (closureScope, d') <- liftIO (readIORef closure)
    modifyContext (closureScope :)
    setClosureDepth (Just d')

    -- Call
    ret <- inNewScope $ do
        modifyContext (declareVar "rec" f) -- Self-reference for recursion
        modifyContext (declareVar name x) -- Function argument
        comp

    -- Update closure
    liftIO . writeIORef closure =<< (,d') . head . context <$> get

    -- Restore variable context (remove closure scope)
    modifyContext tail
    setClosureDepth d

    return ret
interpret (If cond t f) = do
    Bool b <- interpret cond
    interpret (if b then t else f)
interpret (Declaration name e) = do
    val <- interpret e
    modifyContext (declareVar name val)
    return val
interpret (Variable name) = getVar name
interpret (Nonlocal name) = do
    closureDepth <$> get >>= \case
        Just d  -> modifyContext (makeNonlocal d name)
        Nothing -> error (printf "can't make %s nonlocal; not in a lambda" name)
    return Unit
interpret (EraseVar name) = do
    modifyContext (eraseVar name)
    return Unit

interpret UnitLiteral = return Unit
interpret (BoolLiteral b) = return (Bool b)
interpret (IntLiteral n) = return (Int n)
interpret (StringLiteral s) = return (String s)
interpret (AssignmentLiteral name) =
    functionLiteral $ \val -> do
        modifyContext (assignVar name val)
        return val
interpret (OperatorLiteral Plus) =
    functionLiteral $ \(Int b) ->
        functionLiteral $ \(Int a) ->
            return (Int (a + b))
interpret (OperatorLiteral Minus) =
    functionLiteral $ \(Int b) ->
        functionLiteral $ \(Int a) ->
            return (Int (a - b))
interpret (OperatorLiteral Mult) =
    functionLiteral $ \(Int b) ->
        functionLiteral $ \(Int a) ->
            return (Int (a * b))
interpret (OperatorLiteral Pow) =
    functionLiteral $ \(Int b) ->
        functionLiteral $ \(Int a) ->
            return (Int (a ^ b))
interpret (OperatorLiteral Equal) =
    functionLiteral $ \(Int b) ->
        functionLiteral $ \(Int a) ->
            return (Bool (a == b))
interpret (OperatorLiteral Less) =
    functionLiteral $ \(Int b) ->
        functionLiteral $ \(Int a) ->
            return (Bool (a < b))
interpret (OperatorLiteral Concat) =
    functionLiteral $ \(String b) ->
        functionLiteral $ \(String a) ->
            return (String (a ++ b))
interpret (OperatorLiteral Print) =
    functionLiteral $ \val -> do
        liftIO $ case val of
            String s -> putStrLn s
            _        -> print val
        return val
interpret (OperatorLiteral Debug) = do
    liftIO . printf "State: %s\n" . show =<< get
    return Unit


functionLiteral :: (Value -> Computation Value) -> Computation Value
functionLiteral f = do
    closure <- liftIO (newIORef (M.empty, 0))
    return $ Function closure "arg" (getVar "arg" >>= f)

modifyContext :: (VariableContext -> VariableContext) -> Computation ()
modifyContext f =
    modify (\state@ProgramState{context} -> state{context = f context})

setClosureDepth :: Maybe Int -> Computation ()
setClosureDepth d =
    modify (\state -> state{closureDepth = d})

inNewScope :: Computation a -> Computation a
inNewScope comp = do
    modifyContext (M.empty :)
    x <- comp
    modifyContext tail
    return x

getVar :: String -> Computation Value
getVar name =
    (listToMaybe . catMaybes . map (M.!? name) . context <$> get)
    >>= \case
        Just val -> return val
        Nothing -> liftIO . ioError . userError $
                     printf "variable %s not found" name

declareVar :: String -> Value -> VariableContext -> VariableContext
declareVar name val [] =
    error (printf "can't declare %s = %s: no scope" name (show val))
declareVar name val (scope:t) = M.insert name val scope : t
   -- case scope M.!? name of
   --      Just _  -> error (printf "variable %s already exists" name)
   --      Nothing -> M.insert name val scope : t

assignVar :: String -> Value -> VariableContext -> VariableContext
assignVar name val [] =
    error (printf "can't assign %s = %s: not found" name (show val))
assignVar name val (scope:t) =
    case scope M.!? name of
        Just _  -> M.insert name val scope : t
        Nothing -> scope : assignVar name val t

eraseVar :: String -> VariableContext -> VariableContext
eraseVar name [] = error (printf "can't erase %s: not found" name)
eraseVar name (scope:t) =
    case scope M.!? name of
        Just _  -> M.delete name scope : t
        Nothing -> scope : eraseVar name t

makeNonlocal :: Int -> String -> VariableContext -> VariableContext
makeNonlocal depth name ctxt =
    let layers_to_erase = max 2 (length ctxt - depth)
        -- at minimum locals and closure
    in helper layers_to_erase ctxt
  where
    helper 0 ctxt = ctxt
    helper depth (scope:t) =
        case scope M.!? name of
            Just _  -> M.delete name scope : helper (depth - 1) t
            Nothing ->               scope : helper (depth - 1) t


flattenContext :: VariableContext -> Scope
flattenContext = M.unions

mkClosure :: Computation Closure
mkClosure = do
    ctxt <- context <$> get
    liftIO (newIORef (flattenContext ctxt, length ctxt))
