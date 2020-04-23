module Eval where

import Data.List
import Algorithm

evalAlgorithm :: Algorithm -> (String, Integer) -> (String, Integer)
evalAlgorithm (Algorithm n chars insts) input =
  if not . all (`elem` chars) $ fst input
    then error $ "Invalid input: " ++ fst input
    else doAlgo input
  where
    doAlgo :: (String, Integer) -> (String, Integer)
    doAlgo (sigma, j)
      | j == n = (sigma, j)
      | otherwise = doAlgo $ evalInstruction (findInstruction j insts) (sigma, j)

scanAlgorithm :: Algorithm -> (String, Integer) -> [(String, Integer)]
scanAlgorithm (Algorithm n chars insts) input = 
  doAlgo validInput
  where
    doAlgo :: (String, Integer) -> [(String, Integer)]
    doAlgo (sigma, j)
      | j == n = [(sigma, j)]
      | otherwise = 
        let nextState = evalInstruction (findInstruction j insts) (sigma, j)
         in nextState : doAlgo nextState

    validInput :: (String, Integer)
    validInput = 
      if not . all (`elem` chars) $ fst input
        then error $ "Invalid input: " ++ fst input
        else input

evalStep :: Algorithm -> (String, Integer) -> (String, Integer)
evalStep (Algorithm n chars insts) (sigma, j) 
  | j == n = (sigma, j)
  | otherwise = evalInstruction (findInstruction j insts) (sigma, j)

findInstruction :: Integer -> [Instruction] -> Instruction
findInstruction j insts =
  case filter ((== j) . instJ) insts of
    [] -> error $ "No instruction with j: " ++ show j
    (inst:_) -> inst

evalInstruction :: Instruction -> (String, Integer) -> (String, Integer)
evalInstruction (Instruction _ theta phi b a) (sigma, _) =
  case splitTheta theta sigma of
    Nothing -> (sigma, a)
    Just (alpha, omega) -> (alpha ++ phi ++ omega, b)

splitTheta :: String -> String -> Maybe (String, String)
splitTheta [] [] = Just ("", "")
splitTheta theta [] = Nothing
splitTheta theta string@(c:cs) =
  case stripPrefix theta string of
    Just stripped -> Just ("", stripped)
    Nothing -> do
      (before, after) <- splitTheta theta cs
      return (c : before, after)
