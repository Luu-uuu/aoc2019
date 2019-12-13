{-# LANGUAGE NoMonomorphismRestriction #-}

import System.IO
import System.Environment
import Numeric
import Data.List.Index
import Data.List.Split
import Debug.Trace

isValue :: [Int] -> IO ()
isValue code = do
    case (head code) of
      19690720 ->  print(code)

generateAllPermutations :: [Int] -> [[Int]]
generateAllPermutations code = do
    let base = (drop 4 code)
    let possibleInts = [ [1,x,3,4] | x <- [0..99]]
    [ x++base | x <- possibleInts ]

executeCode :: Int -> [Int] -> [Int]
executeCode pc code = do
    case word of
      [1, v1, v2, loc] -> executeCode (pc + 4) (setAt loc ((code!!v1)+(code!!v2)) code)
      [2, v1, v2, loc] -> executeCode (pc + 4) (setAt loc ((code!!v1)*(code!!v2)) code)
      99 : _  -> code
    where
        word = (take 4 (drop pc code))

main = do
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let unparsedCode = [if c == ',' then ' ' else c | c <- contents]
    let code = words unparsedCode
    let intCodes = map read (code) :: [Int]
    --print(intCodes)
    --let ranInts = executeCode 0 intCodes
    let all = generateAllPermutations intCodes
    let ranInts = map (executeCode 0) all
    let test = map isValue ranInts
    return ()

