{-# LANGUAGE LambdaCase #-}

module Main where

import Static

import           Data.Generics.Uniplate.Data
import           Language.Lua.Parser
import           Language.Lua.Syntax
import           System.Environment
import           System.Exit
import           System.IO
import qualified Data.Text.IO                as T

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
     mapM_ staticBlock            (universe   chunk)
     mapM_ staticStatement        (universeBi chunk)
     mapM_ staticReturnStatement  (universeBi chunk)
     mapM_ staticFunctionName     (universeBi chunk)
     mapM_ staticVariable         (universeBi chunk)
     mapM_ staticVariableList1    (universeBi chunk)
     mapM_ staticExpression       (universeBi chunk)
     mapM_ staticExpressionList   (universeBi chunk)
     mapM_ staticExpressionList1  (universeBi chunk)
     mapM_ staticPrefixExpression (universeBi chunk)
     mapM_ staticFunctionCall     (universeBi chunk)
     mapM_ staticFunctionArgs     (universeBi chunk)
     mapM_ staticFunctionBody     (universeBi chunk)
     mapM_ staticTableConstructor (universeBi chunk)
     mapM_ staticField            (universeBi chunk)
     mapM_ staticFieldList        (universeBi chunk)
     mapM_ staticBinop            (universeBi chunk)
     mapM_ staticUnop             (universeBi chunk)
