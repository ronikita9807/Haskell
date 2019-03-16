module Main where

import MyProj

main :: IO ()
main = do putStrLn "Hello, what's your name?"
          name <- getLine
          putStrLn ("Hey " ++ name ++ "!")
          putStrLn "Choose you`r profile (1,2,3,4)"
          prof <- getLine
          runMyProj name prof
