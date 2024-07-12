module Main (main) where

import System.Environment (getArgs)
import Parser (readExpr, eval)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
