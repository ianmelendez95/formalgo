module Assemble where

import Algorithm
import Data.List (isSuffixOf, union)
import Data.Char (isDigit)

-- NEW PARSING
  
-- Parsed Instruction
data PInstr = PInstr { pInstrIndex  :: Integer
                     , pInstrLabel  :: Maybe String 
                     , pInstrName   :: String
                     , pInstrParams :: [String]
                     , pInstrSource :: String -- for errors
                     }

-- Assembler Instruction
data AInstr 
  = APrim Prim 
  | ARepa Repa
  | AGoto Goto
  deriving (Show)

data Repa = 
  Repa { repaIndex :: Integer
       , repaLabel :: Maybe String
       , repaTheta :: String
       , repaPhi   :: String
       } deriving (Show)

data Goto =
  Goto { gotoIndex  :: Integer
       , gotoLabel  :: Maybe String
       , gotoGoto   :: String
       } deriving (Show)


primsFromAInstr :: AInstr -> [Prim]
primsFromAInstr (APrim prim) = [prim]
primsFromAInstr (ARepa repa) = [primFromRepa repa]
primsFromAInstr (AGoto goto) = [primFromGoto goto]

aInstrFromPInstr :: PInstr -> AInstr
aInstrFromPInstr instr@(PInstr { pInstrName = name }) 
  | name == "prim" = APrim $ primFromPInstr instr
  | name == "repa" = ARepa $ repaFromPInstr instr
  | name == "goto" = AGoto $ gotoFromPInstr instr
  | otherwise = error $ "Unable to resolve instruction: " ++ pInstrSource instr

primFromRepa :: Repa -> Prim
primFromRepa (Repa { repaIndex = index
                   , repaLabel = label
                   , repaTheta = theta
                   , repaPhi   = phi
                   }) = 
  Prim { primIndex = index
       , primLabel = label
       , primTheta = theta
       , primPhi   = phi
       , primB     = Right 0
       , primA     = Right 1
       }

primFromGoto :: Goto -> Prim
primFromGoto (Goto { gotoIndex = index
                   , gotoLabel = label
                   , gotoGoto  = goto
                   }) = 
  Prim { primIndex = index
       , primLabel = label
       , primTheta = "_"
       , primPhi   = "_"
       , primB     = readPrimAB goto
       , primA     = readPrimAB goto
       }

repaFromPInstr :: PInstr -> Repa
repaFromPInstr (PInstr { pInstrIndex  = index
                       , pInstrLabel  = label
                       , pInstrName   = "repa"
                       , pInstrParams = [theta, phi]
                       }) = 
  Repa { repaIndex = index
       , repaLabel = label
       , repaTheta = readPrimThetaPhi theta
       , repaPhi   = readPrimThetaPhi phi
       }
repaFromPInstr instr = 
  error $ "Malformed repa instruction -" 
            ++ " expect 'repa theta phi': " 
            ++ pInstrSource instr

gotoFromPInstr :: PInstr -> Goto
gotoFromPInstr (PInstr { pInstrIndex  = index
                       , pInstrLabel  = label
                       , pInstrName   = "goto"
                       , pInstrParams = [goto]
                       }) = 
  Goto { gotoIndex = index
       , gotoLabel = label
       , gotoGoto  = goto
       }
gotoFromPInstr instr = 
  error $ "Malformed goto instruction -" 
            ++ " expect 'goto (offset | label)': " 
            ++ pInstrSource instr

primFromPInstr :: PInstr -> Prim
primFromPInstr (PInstr { pInstrIndex  = index
                       , pInstrLabel  = label
                       , pInstrName   = "prim"
                       , pInstrParams = [theta, phi, b, a]
                       }) = 
  Prim { primIndex = index
       , primLabel = label
       , primTheta = readPrimThetaPhi theta
       , primPhi   = readPrimThetaPhi phi
       , primB     = readPrimAB b
       , primA     = readPrimAB a
       }
primFromPInstr instr = 
  error $ "Malformed prim instruction -" 
            ++ " expect 'prim theta phi b-offset a-offset': " 
            ++ pInstrSource instr

readPInstrs :: [String] -> [PInstr]
readPInstrs instrLines = map (uncurry readPInstr) $ zip [0..] instrLines

readPInstr :: Integer -> String -> PInstr
readPInstr index instrStr = 
  case words instrStr of
    (w1:w2:ws) -> 
      case extractLabel w1 of
        Nothing -> PInstr { pInstrIndex  = index
                          , pInstrLabel  = Nothing
                          , pInstrName   = w1
                          , pInstrParams = (w2:ws)
                          , pInstrSource = instrStr
                          }
        label   -> PInstr { pInstrIndex  = index
                          , pInstrLabel  = label 
                          , pInstrName   = w2
                          , pInstrParams = ws
                          , pInstrSource = instrStr
                          }
    _ -> error $ "Unable to read instruction: " ++ instrStr

-- Prim = all of the information we can deduce during parsing
  
data Prim = Prim
  { primIndex :: Integer
  , primLabel :: Maybe String
  , primTheta :: String
  , primPhi   :: String
  , primB     :: Either String Integer -- label | offset
  , primA     :: Either String Integer -- label | offset
  } deriving (Show)

primHasLabel :: String -> Prim -> Bool
primHasLabel label (Prim { primLabel = (Just pLabel) }) = label == pLabel
primHasLabel _ _ = False

-- Assembler

assemble :: String -> Algorithm
assemble content = 
  let pInstrs = readPInstrs . lines $ content
      aInstrs = map aInstrFromPInstr pInstrs
      prims   = concatMap primsFromAInstr aInstrs

      instructions = primsToInstructions prims
      n    = fromIntegral $ length instructions
      aSet = unionAll $ map aSetFromInstruction instructions
   in Algorithm n aSet instructions

unionAll :: Eq a => [[a]] -> [a]
unionAll = foldr union []

aSetFromInstruction :: Instruction -> String
aSetFromInstruction inst = union (instTheta inst) (instPhi inst)

-- Prim to Instruction

primsToInstructions :: [Prim] -> [Instruction]
primsToInstructions prims = map primToInstruction prims
  where
    -- where func because it depends on finding labels in prims
    primToInstruction :: Prim -> Instruction
    primToInstruction (Prim { primIndex = index
                            , primLabel = label
                            , primTheta = theta
                            , primPhi = phi
                            , primB = b
                            , primA = a 
                            }) = 
      Instruction 
        { instJ = index
        , instTheta = theta
        , instPhi = phi
        , instB = primToInstrAB ((+ index) <$> b) -- will add the index if integer offset
        , instA = primToInstrAB ((+ index) <$> a)
        }

    primToInstrAB :: Either String Integer -> Integer
    primToInstrAB (Right j) = j
    primToInstrAB (Left label) = labelToJ label

    labelToJ :: String -> Integer
    labelToJ label = findIndexForLabel label prims

-- Prim labels

findIndexForLabel :: String -> [Prim] -> Integer
findIndexForLabel label = primIndex . findPrimByLabel label

findPrimByLabel :: String -> [Prim] -> Prim
findPrimByLabel label prims = 
  case find (primHasLabel label) prims of
    Nothing -> error $ "No instruction for label: " ++ label
    Just prim -> prim

find :: (a -> Bool) -> [a] -> Maybe a
find pred xs = 
  case filter pred xs of
    [] -> Nothing
    (x:_) -> Just x

-- Prim reading

readPrims :: [String] -> [Prim]
readPrims primLines = map (uncurry readPrim) $ zip [0..] primLines

readPrim :: Integer -> String -> Prim
readPrim index = readPrimWords index . words

readPrimWords :: Integer -> [String] -> Prim
readPrimWords index instrWs@[label, name, theta, phi, b, a] = 
  case extractLabel label of
    Nothing -> error $ "Invalid label: " ++ unwords instrWs
    label   -> (readPrimWords index $ tail instrWs) { primLabel = label }
readPrimWords index instrWs@["prim", theta, phi, b, a] =
  Prim { primIndex = index
       , primLabel = Nothing
       , primTheta = readPrimThetaPhi theta
       , primPhi = readPrimThetaPhi phi
       , primB = readPrimAB b
       , primA = readPrimAB a
       }
readPrimWords _ instrWs = error $ "Not a valid instruction: " ++ unwords instrWs

readPrimThetaPhi :: String -> String
readPrimThetaPhi str =
  if str == "_"
     then ""
     else str

readPrimAB :: String -> Either String Integer
readPrimAB str = 
  if isDigit . head $ str
     then Right $ read str
     else Left str

extractLabel :: String -> Maybe String
extractLabel [] = Nothing
extractLabel (c:[]) = if c == ':' then Just [] else Nothing 
extractLabel (c:cs) = (c:) <$> extractLabel cs
