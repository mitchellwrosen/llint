{-# LANGUAGE LambdaCase #-}

module Main where

import Queries
import Static

import           Language.Lua.Parser
import           Language.Lua.Syntax
import           System.Environment
import           System.Exit
import           System.IO
import qualified Data.Text.IO        as T

main :: IO ()
main = getArgs >>= \case
    [file] -> do
        chunk <- parseLua file <$> readFile file

        mapM_ (T.putStrLn . showSuggestion) $
            runStatic (StyleGuide 4) (staticAnalysis chunk)
    _ -> do
        progName <- getProgName
        hPutStrLn stderr $ "Usage: " ++ progName ++ " <filename>"
        exitFailure

staticAnalysis :: Chunk NodeInfo -> Static ()
staticAnalysis chunk = do
     mapM_ staticBlock            (allBlocks            chunk)
     mapM_ staticStatement        (allStatements        chunk)
     mapM_ staticReturnStatement  (allReturnStatements  chunk)
     mapM_ staticFunctionName     (allFunctionNames     chunk)
     mapM_ staticVariable         (allVariables         chunk)
     mapM_ staticVariableList1    (allVariableList1s    chunk)
     mapM_ staticExpression       (allExpressions       chunk)
     mapM_ staticExpressionList   (allExpressionLists   chunk)
     mapM_ staticExpressionList1  (allExpressionList1s  chunk)
     mapM_ staticPrefixExpression (allPrefixExpressions chunk)
     mapM_ staticFunctionCall     (allFunctionCalls     chunk)
     mapM_ staticFunctionArgs     (allFunctionArgs      chunk)
     mapM_ staticFunctionBody     (allFunctionBodies    chunk)
     mapM_ staticTableConstructor (allTableConstructors chunk)
     mapM_ staticField            (allFields            chunk)
     mapM_ staticFieldList        (allFieldLists        chunk)
     mapM_ staticBinop            (allBinops            chunk)
     mapM_ staticUnop             (allUnops             chunk)
