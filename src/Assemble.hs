module Assemble where

import System.IO
import Data.Char (isSpace)
import Data.List (union)
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

-- READ

readAlgorithm :: [String] -> Algorithm
readAlgorithm instrLines = assemble $ map (readPrim . words) instrLines

readPrim :: [String] -> Prim
readPrim instr@[primStr, theta, phi, b, a] 
  | primStr /= "prim" = error $ "Not a prim instruction: " ++ concat instr
  | otherwise = Prim (tpFromString theta) (tpFromString phi) (read b) (read a)

-- ASSEMBLE

assemble :: [Prim] -> Algorithm
assemble prims =
  let aSet = unionAll $ map aSetFromPrim prims
      instructions = assembleInstructions 0 prims
   in Algorithm (fromIntegral $ length instructions) aSet instructions

unionAll :: Eq a => [[a]] -> [a]
unionAll = foldr union []

aSetFromPrim :: Prim -> String
aSetFromPrim prim = union (primTheta prim) (primPhi prim)

assembleInstructions :: Integer -> [Prim] -> [Instruction]
assembleInstructions _ [] = []
assembleInstructions instrNum ((Prim theta phi b a):prims) = 
  (Instruction instrNum 
               theta
               phi
               (instrNum + b) 
               (instrNum + a)) : (assembleInstructions (instrNum + 1) prims)
