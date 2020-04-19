module Assemble where

import System.IO
import Data.Char (isSpace, isDigit)
import Data.List (union, isSuffixOf)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Parser

import Algorithm

data Header = Header
  { headerA    :: String
  } deriving (Show)

data Prim = Prim
  { primTheta :: String
  , primPhi   :: String
  , primB     :: String
  , primA     :: String
  } deriving (Show)

-- IO 

assembleFile :: String -> IO Algorithm
assembleFile filePath = (readAlgorithm . lines) <$> readFile filePath

-- READ

readAlgorithm :: [String] -> Algorithm
readAlgorithm instrLines = assemble $ doRead instrLines

-- NEW READ

doRead :: [String] -> [Instruction]
doRead instrLines = 
  let indexedPrims = indexPrims . parsePrims $ instrLines
   in primsToInstructions (extractLabelMap indexedPrims) indexedPrims

parsePrims :: [String] -> [(Maybe String, Prim)] -- (maybe label, prim)
parsePrims = map (readPrimTokens . words)

indexPrims :: [(Maybe String, Prim)] -> [(Integer, (Maybe String, Prim))]
indexPrims = zip [0..]

extractLabelMap :: [(Integer, (Maybe String, Prim))] 
                -> [(String, Integer)]
extractLabelMap = mapMaybe extractLabelEntry

primsToInstructions :: [(String, Integer)] 
                    -> [(Integer, (Maybe String, Prim))] 
                    -> [Instruction]
primsToInstructions labelMap = map (primToInstruction labelMap)

extractLabelEntry :: (Integer, (Maybe String, Prim)) -> Maybe (String, Integer)
extractLabelEntry (index, (Just label, _)) = Just (label, index)
extractLabelEntry _ = Nothing

primToInstruction :: [(String, Integer)] 
                  -> (Integer, (Maybe String, Prim)) 
                  -> Instruction
primToInstruction labelMap (index, (_, Prim theta phi b a)) = 
  Instruction index theta phi (resolveOffset b) (resolveOffset a)
  where
    resolveOffset :: String -> Integer
    resolveOffset offsetStr@(c:cs) = 
      if isDigit c 
         then readInt offsetStr
         else lookupLabel offsetStr

    lookupLabel :: String -> Integer
    lookupLabel label = 
      case lookup label labelMap of
        Nothing -> error $ "Label does not exist: " ++ label
        Just index -> index

readPrimTokens :: [String] -> (Maybe String, Prim)
readPrimTokens instr@[label, primStr, theta, phi, b, a] 
  | ":" `isSuffixOf` label = 
    (\(_, prim) -> (Just $ trimLast label, prim)) $ readPrimTokens (tail instr)
  | otherwise = error $ "Malformed label: " ++ label
readPrimTokens instr@[primStr, theta, phi, b, a] 
  | primStr /= "prim" = error $ "Not a prim instruction: " ++ concat instr
  | otherwise = (Nothing, Prim (tpFromString theta) 
                               (tpFromString phi) 
                               b 
                               a)

trimLast :: String -> String
trimLast [] = []
trimLast (x:[]) = []
trimLast (x:xs) = x : trimLast xs

-- ASSEMBLE

assemble :: [Instruction] -> Algorithm
assemble insts =
  let aSet = unionAll $ map aSetFromInstruction insts
   in Algorithm (fromIntegral $ length insts) aSet insts

unionAll :: Eq a => [[a]] -> [a]
unionAll = foldr union []

aSetFromInstruction :: Instruction -> String
aSetFromInstruction inst = union (instTheta inst) (instPhi inst)

readInt :: String -> Integer
readInt str = 
  case readMaybe str of
    Nothing -> error $ "Unable to read integer: " ++ str
    Just num -> num

find :: (a -> Bool) -> [a] -> Maybe a
find pred xs = 
  case filter pred xs of 
    [] -> Nothing
    (x:_) -> Just x

