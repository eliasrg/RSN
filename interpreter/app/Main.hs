{-# LANGUAGE ViewPatterns #-}

module Main where

import Parser
import AST
import Interpreter

import Control.Monad
import qualified Data.Map.Strict as M
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    when (not (null args)) $ do
        let filename = head args
        test =<< parseFile filename
        return ()


test :: (Show a) => Either a Expression -> IO Value
test (either (error . show) id -> expr) = do
    let comp = interpret expr
    val <- run comp
    return val
