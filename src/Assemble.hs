module Assemble where

import Eval (Algorithm (..), Instruction (..))
import Parser
import Data.Char (isSpace)

data Header = Header
  { headerN    :: Integer
  , headerA    :: String
  }

data Prim = Prim
  { primTheta :: String
  , primPhi   :: String
  , primB     :: Integer
  , primA     :: Integer
  }

-- ASSEMBLE

assemble :: Header -> [Prim] -> Algorithm
assemble (Header n a) prims =
  let instructions = assembleInstructions 0 prims
   in Algorithm n a a instructions

assembleInstructions :: Integer -> [Prim] -> [Instruction]
assembleInstructions instrNum ((Prim theta phi b a):prims) = 
  (Instruction instrNum 
               (tpToInstruction theta) 
               (tpToInstruction phi) 
               (instrNum + b) 
               (instrNum + a)) : (assembleInstructions (instrNum + 1) prims)

-- TO STRINg

algorithmToString :: Algorithm -> String
algorithmToString (Algorithm n a endA insts) = 
  let header = unwords $[show n, a, endA] 
      instructionsStr = map instructionToString insts
   in unlines $ header : instructionsStr
  
instructionToString :: Instruction -> String
instructionToString (Instruction j theta phi b a) =
  unwords $ [show j, tpToString theta, tpToString phi, show b, show a]

tpToString :: String -> String
tpToString thetaOrPhi = if null thetaOrPhi then "_" else thetaOrPhi 

tpToInstruction :: String -> String
tpToInstruction thetaOrPhi = if thetaOrPhi == "_" then "" else thetaOrPhi
