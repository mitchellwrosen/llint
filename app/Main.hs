{-# LANGUAGE LambdaCase #-}

module Main where

import Queries
import Style

import Language.Lua.Parser
import Language.Lua.Syntax
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = getArgs >>= \case
    [file] -> do
        chunk <- parseLua file <$> readFile file
        mapM_ putStrLn $ runStyler (StyleGuide 4) (styler chunk)
    _ -> do
        progName <- getProgName
        hPutStrLn stderr $ "Usage: " ++ progName ++ " <filename>"
        exitFailure

styler :: Chunk NodeInfo -> Styler ()
styler chunk = do
     mapM_ styleCheckBlock     (allBlocks chunk)
     mapM_ styleCheckStatement (allStatements chunk)
