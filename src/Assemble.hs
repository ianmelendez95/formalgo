module Assemble where

import Algorithm
import Data.List (isSuffixOf, union)
import Data.Char (isDigit)

-- 
-- Assembly
--

assemble :: String -> Algorithm
assemble content = 
  let pInstrs :: [IL PInstr]
      pInstrs = readPInstrs . lines $ content

      aInstrs :: [IL AInstr]
      aInstrs = map (aInstrFromPInstr <$>) pInstrs

      prims :: [IL Prim]
      prims   = map (primFromAInstr <$>) aInstrs

      instructions :: [Instruction]
      instructions = primsToInstructions prims

      n    = fromIntegral $ length instructions
      aSet = unionAll $ map aSetFromInstruction instructions
   in Algorithm n aSet instructions

unionAll :: Eq a => [[a]] -> [a]
unionAll = foldr union []

aSetFromInstruction :: Instruction -> String
aSetFromInstruction inst = union (instTheta inst) (instPhi inst)

-- Prim to Instruction

primsToInstructions :: [IL Prim] -> [Instruction]
primsToInstructions prims = map primToInstruction prims
  where
    -- where func because it depends on finding labels in prims
    primToInstruction :: IL Prim -> Instruction
    primToInstruction (IL index _ (Prim { primTheta = theta
                                        , primPhi = phi
                                        , primB = b
                                        , primA = a 
                                        })) = 
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

-- Component reading

readInstrThetaPhi :: String -> String
readInstrThetaPhi str =
  if str == "_"
     then ""
     else str

readInstrAB :: String -> Either String Integer
readInstrAB str = 
  if isDigit . head $ str
     then Right $ read str
     else Left str

--
-- Instructions
--

-- Indexed, Labeled Data
  
data IL a = 
  IL { ilIndex :: Integer 
     , ilLabel :: Maybe String
     , ilData  :: a }

instance Functor IL where
  fmap f (IL i l x) = IL i l (f x)

ilHasLabel :: String -> IL a -> Bool
ilHasLabel label = maybe False (label ==) . ilLabel

findIndexForLabel :: String -> [IL a] -> Integer
findIndexForLabel label = ilIndex . findILByLabel label

findILByLabel :: String -> [IL a] -> IL a
findILByLabel label = maybe throwError id . find (ilHasLabel label)
  where
    throwError = error $ "No instruction for label: " ++ label

find :: (a -> Bool) -> [a] -> Maybe a
find pred xs = 
  case filter pred xs of
    [] -> Nothing
    (x:_) -> Just x

-- Parsed Instruction
  
data PInstr = PInstr { pInstrName   :: String
                     , pInstrParams :: [String]
                     , pInstrSource :: String -- for errors
                     }

readPInstrs :: [String] -> [IL PInstr]
readPInstrs instrLines = map (uncurry readPInstr) $ zip [0..] instrLines

readPInstr :: Integer -> String -> IL PInstr
readPInstr index instrStr = 
  case words instrStr of
    (w1:w2:ws) -> 
      case extractLabel w1 of
        Nothing -> IL index Nothing (PInstr { pInstrName   = w1
                                            , pInstrParams = (w2:ws)
                                            , pInstrSource = instrStr
                                            })
        label   -> IL index label   (PInstr { pInstrName   = w2
                                            , pInstrParams = ws
                                            , pInstrSource = instrStr
                                            })
    _ -> error $ "Unable to read instruction: " ++ instrStr

extractLabel :: String -> Maybe String
extractLabel [] = Nothing
extractLabel (c:[]) = if c == ':' then Just [] else Nothing 
extractLabel (c:cs) = (c:) <$> extractLabel cs

-- Assembler Instructions
  
data AInstr 
  = APrim Prim 
  | ARepa Repa
  | AGoto Goto
  deriving (Show)

aInstrFromPInstr :: PInstr -> AInstr
aInstrFromPInstr instr@(PInstr { pInstrName = name }) 
  | name == "prim" = APrim $ primFromPInstr instr
  | name == "repa" = ARepa $ repaFromPInstr instr
  | name == "goto" = AGoto $ gotoFromPInstr instr
  | otherwise = error $ "Unable to resolve instruction: " ++ pInstrSource instr

primFromAInstr :: AInstr -> Prim
primFromAInstr (APrim prim) = prim
primFromAInstr (ARepa repa) = primFromRepa repa
primFromAInstr (AGoto goto) = primFromGoto goto

-- REPA

data Repa = 
  Repa { repaTheta :: String
       , repaPhi   :: String
       } deriving (Show)

repaFromPInstr :: PInstr -> Repa
repaFromPInstr (PInstr { pInstrName   = "repa"
                       , pInstrParams = [theta, phi]
                       }) = 
  Repa { repaTheta = readInstrThetaPhi theta
       , repaPhi   = readInstrThetaPhi phi
       }
repaFromPInstr instr = 
  error $ "Malformed repa instruction -" 
            ++ " expect 'repa theta phi': " 
            ++ pInstrSource instr

primFromRepa :: Repa -> Prim
primFromRepa (Repa { repaTheta = theta
                   , repaPhi   = phi
                   }) = 
  Prim { primTheta = theta
       , primPhi   = phi
       , primB     = Right 0
       , primA     = Right 1
       }

-- GOTO

data Goto =
  Goto { gotoGoto   :: String } deriving (Show)

gotoFromPInstr :: PInstr -> Goto
gotoFromPInstr (PInstr { pInstrName   = "goto", pInstrParams = [goto] }) = 
  Goto { gotoGoto  = goto }
gotoFromPInstr instr = 
  error $ "Malformed goto instruction -" 
            ++ " expect 'goto (offset | label)': " 
            ++ pInstrSource instr

primFromGoto :: Goto -> Prim
primFromGoto (Goto { gotoGoto  = goto }) = 
  Prim { primTheta = "_"
       , primPhi   = "_"
       , primB     = readInstrAB goto
       , primA     = readInstrAB goto
       }

-- PRIM

data Prim = Prim
  { primTheta :: String
  , primPhi   :: String
  , primB     :: Either String Integer -- label | offset
  , primA     :: Either String Integer -- label | offset
  } deriving (Show)

primFromPInstr :: PInstr -> Prim
primFromPInstr (PInstr { pInstrName   = "prim"
                       , pInstrParams = [theta, phi, b, a]
                       }) = 
  Prim { primTheta = readInstrThetaPhi theta
       , primPhi   = readInstrThetaPhi phi
       , primB     = readInstrAB b
       , primA     = readInstrAB a
       }
primFromPInstr instr = 
  error $ "Malformed prim instruction -" 
            ++ " expect 'prim theta phi b-offset a-offset': " 
            ++ pInstrSource instr

