module Main where

import System.IO (readFile)
import System.Environment (getArgs)
import Data.List (isSuffixOf)
import Eval (Algorithm (..), Instruction (..), evalAlgorithm)

import Assemble

import Lib

main :: IO ()
main = do
  [file] <- validateArgs <$> getArgs
  if ".fa" `isSuffixOf` file
     then executeFaFile file
     else if ".fasm" `isSuffixOf` file
            then assembleFasm file
            else error $ "Not a formalgo file: " ++ file

assembleFasm :: String -> IO ()
assembleFasm file = do
  let newFile = take (length file - 5) file ++ ".fa"
  algo <- assembleFile file
  writeFile newFile (algorithmToString algo)

validateArgs :: [String] -> [String]
validateArgs args = 
  if null args 
     then error $ "Usage: formalgo foo.fa"
     else args

executeFaFile :: String -> IO ()
executeFaFile fileName = do
  algo <- readFaFile fileName
  print $ evalAlgorithm algo ("", 0)

readFaFile :: String -> IO Algorithm
readFaFile fileName = do
  (header:instructionLines) <- lines <$> readFile fileName
  let (n, a, endA) = readFaHeaderWords . words $ header
      instructions = map (readInstructionWords . words) instructionLines
  return $ Algorithm n a endA instructions

readFaHeaderWords :: [String] -> (Integer, String, String)
readFaHeaderWords [n, a, endA] = (read n :: Integer, a, endA)
readFaHeaderWords headerWords = 
  error $ "Unable to parse header: " ++ unwords headerWords

readInstructionWords :: [String] -> Instruction
readInstructionWords [j, theta, phi, b, a] = 
  Instruction (read j :: Integer) 
              (if theta == "_" then "" else theta) 
              (if phi == "_" then "" else phi) 
              (read b :: Integer) 
              (read a :: Integer)
readInstructionWords instWords = 
  error $ "Unable to parse instruction: " ++ unwords instWords


