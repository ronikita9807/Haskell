module Main where

import MyProj
import ImgWork
import System.Random
import Data.Time

main :: IO ()
main = do putStrLn "Hello, what's your name?"
          name <- getLine
          putStrLn ("Welcome " ++ name ++ "!")
          putStrLn "Choose you`r profile (1,2,3,4)"
          prof <- getLine
          images <- loadImages
          gen <- newStdGen
          tim <- getCurrentTime
          runMyProj tim gen name prof images
