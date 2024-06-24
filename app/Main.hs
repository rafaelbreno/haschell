module Main (main) where

import System.Environment (getArgs)

main :: IO ()
main = do 
  args <- getArgs
  let num1 = read (head args) :: Int
  let num2 = read (args !! 1) :: Int
  putStrLn "Name:"
  name <- getLine
  putStrLn ("Hello, " ++ name)
  putStrLn (show num1 ++ " + " ++ show num2 ++ " = " ++ show (num1 + num2))
