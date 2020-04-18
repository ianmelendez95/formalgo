module Algorithm where

data Algorithm = Algorithm
  { algoN     :: Integer
  , algoA     :: String
  , algoInsts :: [Instruction]
  } deriving (Show)

data Instruction = Instruction
  { instJ     :: Integer
  , instTheta :: String
  , instPhi   :: String
  , instB     :: Integer
  , instA     :: Integer
  } deriving (Show)

-- TO STRING

algorithmToString :: Algorithm -> String
algorithmToString (Algorithm n a insts) = 
  let header = unwords [show n, a] 
      instructionsStr = map instructionToString insts
   in unlines $ header : instructionsStr
  
instructionToString :: Instruction -> String
instructionToString (Instruction j theta phi b a) =
  unwords $ [show j, tpToString theta, tpToString phi, show b, show a]

-- FROM STRING

algorithmFromString :: String -> Algorithm
algorithmFromString str = 
  let (header:instructionLines) = lines str
      (n, a) = readHeaderWords . words $ header
      instructions = map (readInstructionWords . words) instructionLines
   in Algorithm n a instructions

readHeaderWords :: [String] -> (Integer, String)
readHeaderWords [n, a] = (read n :: Integer, a)
readHeaderWords headerWords = 
  error $ "Unable to parse header: " ++ unwords headerWords

readInstructionWords :: [String] -> Instruction
readInstructionWords [j, theta, phi, b, a] = 
  Instruction (read j :: Integer) 
              (tpFromString theta) 
              (tpFromString phi) 
              (read b :: Integer) 
              (read a :: Integer)
readInstructionWords instWords = 
  error $ "Unable to parse instruction: " ++ unwords instWords

tpToString :: String -> String
tpToString thetaOrPhi = if null thetaOrPhi then "_" else thetaOrPhi 

tpFromString :: String -> String
tpFromString thetaOrPhi = if thetaOrPhi == "_" then "" else thetaOrPhi

