module FormalAlgorithm where

import Data.List

-- MULT 

evalMult :: Integer -> Integer -> Integer
evalMult x y = 
  let string = replicate (fromInteger x) 'a' ++ replicate (fromInteger y) 'b'
   in toInteger . length . fst $ evalAlgorithm multAlgorithm (string, 0)

multAlgorithm :: Algorithm
multAlgorithm = Algorithm 5 "abcd"
  [ Instruction 0 "b"  ""   1 3 
  , Instruction 1 "a"  "cd" 1 2
  , Instruction 2 "d"  "a"  2 0
  , Instruction 3 "a"  ""   3 4
  , Instruction 4 "c"  "a"  4 5
  ]

-- REMAINDER

evalRem :: Integer -> Integer -> Integer
evalRem x y = 
  let string = replicate (fromInteger x) 'a' ++ replicate (fromInteger y) 'b'
   in toInteger . length . fst $ evalAlgorithm remAlgorithm (string, 10)

remAlgorithm :: Algorithm
remAlgorithm = Algorithm 1000 "ab"
  [ Instruction 10 "ab" ""   20 30   -- remove ab
  , Instruction 20 ""   "c"  10  10  -- prepend c

  , Instruction 30 "b"  "b"  40 60   -- has b?

  -- has b, #b > #a, a is remainder
  , Instruction 40 "b"  ""   40 50   -- remove b
  , Instruction 50 "c"  "a"  50 1000 -- c -> a, done

  , Instruction 60 "ca" "ac" 60 70   -- sort ac: cccaa -> aaccc 
  , Instruction 70 "c"  "b"  70 10   -- c -> b, restart
  ]

-- IS EQUAL
-- a = true, "" = false

evalIsEqual :: Integer -> Integer -> Bool
evalIsEqual x y = 
  let string = replicate (fromInteger x) 'a' ++ replicate (fromInteger y) 'b'
   in null . fst $ evalAlgorithm isEqualAlgorithm (string, 10)

isEqualAlgorithm :: Algorithm
isEqualAlgorithm = Algorithm 1000 "ab"
  [ Instruction 10 "ab" ""  10 20   -- remove ab
  , Instruction 20 "b"  ""  30 50   -- b?

  -- b left
  , Instruction 30 "b"  ""  30 40      -- remove bs
  , Instruction 40 ""   "a" 1000 1000  -- add a
  
  , Instruction 50 "a"  "a" 60 1000 -- a? 
  , Instruction 60 "aa" "a" 60 1000 -- reduce to a single a 
  ]

-- ADD

evalAdd :: Integer -> Integer -> Integer
evalAdd x y = 
  let string = replicate (fromInteger x) 'a' ++ replicate (fromInteger y) 'b'
   in toInteger . length . fst $ evalAlgorithm addAlgorithm (string, 0)

addAlgorithm :: Algorithm
addAlgorithm = Algorithm 1 "ab"
  [ Instruction 0 "b" "a" 0 1 ]

-- DIFF

evalDiff :: Integer -> Integer -> Integer
evalDiff x y = 
  let string = replicate (fromInteger x) 'a' ++ replicate (fromInteger y) 'b'
   in toInteger . length . fst $ evalAlgorithm diffAlgorithm (string, 0)

diffAlgorithm :: Algorithm
diffAlgorithm = Algorithm 1 "ab"
  [ Instruction 0 "ab" "" 0 1 ]

-- GCD

evalGcd :: Integer -> Integer -> Integer
evalGcd m n =
  let string = replicate (fromInteger m) 'a' ++ replicate (fromInteger n) 'b'
   in toInteger . length . fst $ evalAlgorithm gcdAlgorithm (string, 0)

{-
   r <- |m-n|
   > r=0? done
   n <- min(m,n)

   m <- n
   n <- r
-}
gcdAlgorithm :: Algorithm
gcdAlgorithm = Algorithm 5 "abc"
  --            j thet phi b a
  [ Instruction 0 "ab" ""  1 2
  , Instruction 1 ""   "c" 0 0
  , Instruction 2 "a"  "b" 2 3
  , Instruction 3 "c"  "a" 3 4
  , Instruction 4 "b"  "b" 0 5]

-- Formal Algorithm

data Algorithm = Algorithm
  { algoEnd   :: Integer
  , algoA     :: String
  , algoInsts :: [Instruction]
  }

data Instruction = Instruction
  { instJ     :: Integer
  , instTheta :: String
  , instPhi   :: String
  , instB     :: Integer
  , instA     :: Integer
  } deriving (Show)

evalAlgorithm :: Algorithm -> (String, Integer) -> (String, Integer)
evalAlgorithm (Algorithm end chars insts) input =
  if not . all (`elem` chars) $ fst input
    then error $ "Invalid input: " ++ fst input
    else doAlgo input
  where
    doAlgo :: (String, Integer) -> (String, Integer)
    doAlgo (sigma, j)
      | j == end = (sigma, j)
      | otherwise = doAlgo $ evalInstruction (findInstruction j insts) (sigma, j)

evalStep :: Algorithm -> (String, Integer) -> (String, Integer)
evalStep (Algorithm end chars insts) (sigma, j) 
  | j == end = (sigma, j)
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
