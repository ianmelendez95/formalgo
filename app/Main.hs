module Main where

import System.IO (readFile)
import System.Environment (getArgs)
import Data.List (isSuffixOf)
import Eval (evalAlgorithm)
import Algorithm

import Assemble

import Lib

main :: IO ()
main = getArgs >>= handleArgs 

handleArgs :: [String] -> IO ()
handleArgs [] = error $ "Usage: formalgo (foo.fa | foo.fasm)"
handleArgs [file] 
  | ".fa" `isSuffixOf` file = executeFa file
  | ".fasm" `isSuffixOf` file = assembleFasm file
  | otherwise = error $ "Unrecognized file: " ++ file

-- .fa

executeFa :: String -> IO ()
executeFa fileName = do
  algo <- algorithmFromString <$> readFile fileName
  putStrLn . fst $ evalAlgorithm algo ("", 0)

-- .fasm

assembleFasm :: String -> IO ()
assembleFasm file = do
  let newFile = take (length file - 5) file ++ ".fa"
  algo <- assembleFile file
  writeFile newFile (algorithmToString algo)


