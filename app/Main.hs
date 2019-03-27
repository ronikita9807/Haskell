module Main where

import MyProj
import ImgWork
import System.Random

main :: IO ()
main = do putStrLn "Hello, what's your name?"
          name <- getLine
          putStrLn ("Welcome " ++ name ++ "!")
          putStrLn "Choose you`r profile (1,2,3,4)"
          prof <- getLine
          images <- loadImages
          gen <- newStdGen
          runMyProj gen name prof images
