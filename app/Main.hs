{-# LANGUAGE LambdaCase #-}

module Main where

import Static

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
            staticAnalysis (StyleGuide 4) chunk

    _ -> do
        progName <- getProgName
        hPutStrLn stderr $ "Usage: " ++ progName ++ " <filename>"
        exitFailure
