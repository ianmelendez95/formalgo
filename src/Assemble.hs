module Assemble where

import System.IO
import Data.Char (isSpace)
import Parser

import Algorithm

data Header = Header
  { headerA    :: String
  } deriving (Show)

data Prim = Prim
  { primTheta :: String
  , primPhi   :: String
  , primB     :: Integer
  , primA     :: Integer
  } deriving (Show)

-- IO 

assembleFile :: String -> IO Algorithm
assembleFile filePath = (readAlgorithm . lines) <$> readFile filePath

readAlgorithm :: [String] -> Algorithm
readAlgorithm (headerLine:instrLines) = 
  let header = readHeader $ words headerLine
      prims = map (readPrim . words) instrLines
   in assemble header prims

readHeader :: [String] -> Header
readHeader [a] = Header a

readPrim :: [String] -> Prim
readPrim instr@[primStr, theta, phi, b, a] 
  | primStr /= "prim" = error $ "Not a prim instruction: " ++ concat instr
  | otherwise = Prim (tpFromString theta) (tpFromString phi) (read b) (read a)

-- ASSEMBLE

assemble :: Header -> [Prim] -> Algorithm
assemble (Header a) prims =
  let instructions = assembleInstructions 0 prims
   in Algorithm (fromIntegral $ length instructions) a instructions

assembleInstructions :: Integer -> [Prim] -> [Instruction]
assembleInstructions _ [] = []
assembleInstructions instrNum ((Prim theta phi b a):prims) = 
  (Instruction instrNum 
               theta
               phi
               (instrNum + b) 
               (instrNum + a)) : (assembleInstructions (instrNum + 1) prims)
