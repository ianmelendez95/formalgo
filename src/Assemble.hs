module Assemble where

import Algorithm
import Data.List (isSuffixOf, union)
import Data.Char (isDigit)

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
  let instructions = primsToInstructions . readPrims . lines $ content
      n = fromIntegral $ length instructions
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
