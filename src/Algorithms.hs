module Algorithms where

import Eval
import Data.List

-- MULT ADD
-- multAdd :: [x,y,z] [p,q] x
-- multAdd = (+ (* x y) z)

{-

mult :: [a,b] [c,d] a
multAlgorithm :: Algorithm
multAlgorithm = Algorithm 5 "abcd"
  [ Instruction 0 "b"  ""   1 3 
  , Instruction 1 "a"  "cd" 1 2
  , Instruction 2 "d"  "a"  2 0
  , Instruction 3 "a"  ""   3 4
  , Instruction 4 "c"  "a"  4 5
  ]

add :: [a,b] [] a -- > [a,e] [] a
addAlgorithm :: Algorithm
addAlgorithm = Algorithm 1 "ab" -- > Algo 6 "ae"
  [ Instruction 0 "b" "a" 0 1   -- > 5 "e" "a" 0 1
  ]

-}

-- REMQUOTE

evalRemQuot :: Integer -> Integer -> (Integer, Integer)
evalRemQuot x y = 
  let input = replicate (fromInteger x) 'a' ++ replicate (fromInteger y) 'b'
      (output, _) = evalAlgorithm remQuotAlgorithm (input, 0)
   in (count 'c' output, count 'd' output)

remQuotAlgorithm :: Algorithm
remQuotAlgorithm = Algorithm 8 "abcd" "cd"
  [ Instruction 0 "ab" ""   1 2 
  , Instruction 1 ""   "c"  0 0

  , Instruction 2 "b"  "b"  7 3 -- b? 

  , Instruction 3 "cd" "dc" 3 4 -- 'ccdda' -> 'ddcca'
  , Instruction 4 ""   "d"  5 5 -- prepend d
  , Instruction 5 "ca" "ac" 5 6 -- 'cccaa' -> 'aaccc'
  , Instruction 6 "c"  "b"  6 0 -- 'aaccc' -> 'aabbb'

  , Instruction 7 "b"  ""   7 8 -- remove b's and done
  ]

-- MULT 

evalMult :: Integer -> Integer -> Integer
evalMult x y = 
  let string = replicate (fromInteger x) 'a' ++ replicate (fromInteger y) 'b'
   in toInteger . length . fst $ evalAlgorithm multAlgorithm (string, 0)

multAlgorithm :: Algorithm
multAlgorithm = Algorithm 5 "abcd" "a"
  [ Instruction 0 "b"  ""   1 3 -- [b] [] []     : [b]   [] []
  , Instruction 1 "a"  "cd" 1 2 -- [a] [] [c,d]  : [a,b] [] [c,d]
  , Instruction 2 "d"  "a"  2 0 -- [d] [] [a]    : [d]   [] [a,c]
  , Instruction 3 "a"  ""   3 4 -- [a] [] []     : 
  , Instruction 4 "c"  "a"  4 5 -- [c] [] [a]
  ]

-- REMAINDER

evalRem :: Integer -> Integer -> Integer
evalRem x y = 
  let string = replicate (fromInteger x) 'a' ++ replicate (fromInteger y) 'b'
   in toInteger . length . fst $ evalAlgorithm remAlgorithm (string, 10)

remAlgorithm :: Algorithm
remAlgorithm = Algorithm 1000 "ab" "a"
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
isEqualAlgorithm = Algorithm 1000 "ab" "a"
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
addAlgorithm = Algorithm 1 "ab" "a"
  [ Instruction 0 "b" "a" 0 1 ]

-- DIFF

evalDiff :: Integer -> Integer -> Integer
evalDiff x y = 
  let string = replicate (fromInteger x) 'a' ++ replicate (fromInteger y) 'b'
   in toInteger . length . fst $ evalAlgorithm diffAlgorithm (string, 0)

diffAlgorithm :: Algorithm
diffAlgorithm = Algorithm 2 "ab" "a"
  [ Instruction 0 "ab" ""  0 1
  , Instruction 1 "b"  "a" 1 2
  ]

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
gcdAlgorithm = Algorithm 5 "abc" "a"
  --            j thet phi b a
  [ Instruction 0 "ab" ""  1 2
  , Instruction 1 ""   "c" 0 0
  , Instruction 2 "a"  "b" 2 3
  , Instruction 3 "c"  "a" 3 4
  , Instruction 4 "b"  "b" 0 5]

count :: Char -> String -> Integer
count c = fromIntegral . length . filter (==c)

