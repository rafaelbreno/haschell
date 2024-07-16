module Main (main) where

import System.Environment (getArgs)
import Parser (readExpr, eval, trapError, extractValue)

main :: IO ()
main = do
  args <- getArgs
  let evaluated = fmap show $ readExpr (head args) >>= eval 
  putStrLn $ extractValue $ trapError evaluated
--main = getArgs >>= print . eval . readExpr . head
