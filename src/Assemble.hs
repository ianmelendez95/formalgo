module Assemble where

import System.IO
import Eval (Algorithm (..), Instruction (..))
import Parser
import Data.Char (isSpace)

data Header = Header
  { headerN    :: Integer
  , headerA    :: String
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
readHeader [n, a] = Header (read n) a

readPrim :: [String] -> Prim
readPrim instr@[primStr, theta, phi, b, a] 
  | primStr /= "prim" = error $ "Not a prim instruction: " ++ concat instr
  | otherwise = Prim (tpFromString theta) (tpFromString phi) (read b) (read a)

-- ASSEMBLE

assemble :: Header -> [Prim] -> Algorithm
assemble (Header n a) prims =
  let instructions = assembleInstructions 0 prims
   in Algorithm n a a instructions

assembleInstructions :: Integer -> [Prim] -> [Instruction]
assembleInstructions _ [] = []
assembleInstructions instrNum ((Prim theta phi b a):prims) = 
  (Instruction instrNum 
               theta
               phi
               (instrNum + b) 
               (instrNum + a)) : (assembleInstructions (instrNum + 1) prims)

-- TO STRING

algorithmToString :: Algorithm -> String
algorithmToString (Algorithm n a endA insts) = 
  let header = unwords [show n, a, endA] 
      instructionsStr = map instructionToString insts
   in unlines $ header : instructionsStr
  
instructionToString :: Instruction -> String
instructionToString (Instruction j theta phi b a) =
  unwords $ [show j, tpToString theta, tpToString phi, show b, show a]

tpToString :: String -> String
tpToString thetaOrPhi = if null thetaOrPhi then "_" else thetaOrPhi 

tpFromString :: String -> String
tpFromString thetaOrPhi = if thetaOrPhi == "_" then "" else thetaOrPhi
