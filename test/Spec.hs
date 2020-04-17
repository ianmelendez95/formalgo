import Hspec

import Eval
import Assemble

main :: IO ()
main = do
  let assembled = assemble header prims
  putStrLn $ algorithmToString assembled

header :: Header
header = Header 5 "abcd"

prims :: [Prim]
prims = 
  [ Prim "b" "_"  1 3
  , Prim "a" "cd" 0 1
  , Prim "d" "a"  0 (-2)
  , Prim "a" "_"  0 1
  , Prim "c" "a"  0 1
  ]
