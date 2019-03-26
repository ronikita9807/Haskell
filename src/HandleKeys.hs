module HandleKeys where

import Data

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.IO.Unsafe

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Juicy

import Graphics.Gloss.Interface.IO.Game
import System.Exit
import Text.Read (readMaybe)

import Control.DeepSeq
import qualified System.IO as SIO

-- | Respond to key events.
handleKeys :: Event -> PongGame -> IO PongGame

-- For an 's' keypress, to reset the game.
handleKeys (EventKey (Char 's') Down _ _) game = return game { ballLoc = (0, -100), ballVel = (randVel game),gameScore = score,
                                                            platformsLoc = (buildPlatforms game), gameOverText = " " }
  where 
    score = if ((gameOverText game) == "GAME OVER!")
                then 0
                else (gameScore game)

-- For an 'p' keypress, to pause the game.
handleKeys (EventKey (Char 'p') Down _ _) game =
  let v = ballVel game
  in if v /= (0,0)
     then return game { ballVelBuf = v, ballVel = (0, 0) }
     else return game

-- For an 'g' keypress, to unpause the game.
handleKeys (EventKey (Char 'g') Down _ _) game = return game { ballVel = ballVelBuf game }

-- For an 't' keypress, to see game rules.
handleKeys (EventKey (Char 't') Down _ _) game = return game { gameState = 1 }

-- For an 'k' keypress, to see game rules.
handleKeys (EventKey (Char 'k') Down _ _) game = return (randColor game)

-- For an 'y' keypress, to see game rules.
handleKeys (EventKey (Char 'y') Down _ _) game = return game { gameState = 0 }

-- For an 'r' keypress, to save your score in the records table.
handleKeys (EventKey (Char 'r') Down _ _) game = do writeFile ("p"++ proFile game ++ ".txt") (" | " ++ (playerName game) ++ "  " ++ show(gameScore game))
                                                    return game


-- For an 'c' keypress, to clear the records board.
handleKeys (EventKey (Char 'c') Down _ _) game = do writeFile ("p1.txt") " "
                                                    writeFile ("p2.txt") " "
                                                    writeFile ("p3.txt") " "
                                                    writeFile ("p4.txt") " "
                                                    return game

-- For an 'o' keypress, to save the game in data base.
handleKeys (EventKey (Char 'o') Down _ _) game = do 
    wHandle <- SIO.openFile ("saves/save"++ proFile game ++ ".txt") SIO.WriteMode
    SIO.hPutStr wHandle (show(level game) ++ "%"
                         ++ show(ballLoc game) ++ "%" 
                         ++ show(ballVelBuf game) ++ "%" 
                         ++ show(ballVel game) ++ "%" 
                         ++ show(platformLoc game) ++ "%" 
                         ++ show(platformsLoc game) ++ "%" 
                         ++ show(platformSizeX game) ++ "%" 
                         ++ show(platformSizeY game) ++ "%" 
                         ++ show(gameScore game) ++ "%" 
                         ++ (gameOverText game) ++ "%" 
                         ++ (playerName game) ++ "%" 
                         ++ (proFile game) ++ "%" 
                         ++ show(pastBallLoc game) ++ "%" 
                         ++ show(secret game) ++ "%" 
                         ++ show(bonusPos game) ++ "%" 
                         ++ show(bonusFlag game) ++ "%")
    SIO.hClose wHandle
    return game

-- For an 'l' keypress, to load the game.
handleKeys (EventKey (Char 'l') Down _ _) game = do handle <- SIO.openFile ("saves/save"++ proFile game ++ ".txt") SIO.ReadMode
                                                    text <- SIO.hGetLine handle
                                                    SIO.hClose handle
                                                    let x1 = read ((parseStr [] text)!!0) :: Integer
                                                        x2 = read ((parseStr [] text)!!1) :: (Float, Float)
                                                        x3 = read ((parseStr [] text)!!2) :: (Float, Float)
                                                        x4 = read ((parseStr [] text)!!3) :: (Float, Float)
                                                        x5 = read ((parseStr [] text)!!4) :: (Float, Float)
                                                        x7 = read ((parseStr [] text)!!5) :: [(Float,Float)]
                                                        x8 = read ((parseStr [] text)!!6) :: Float
                                                        x9 = read ((parseStr [] text)!!7) :: Float
                                                        x10 = read ((parseStr [] text)!!8) :: Score
                                                        x11 = (parseStr [] text)!!9
                                                        x12 = (parseStr [] text)!!10
                                                        x13 = (parseStr [] text)!!11
                                                        x15 = read ((parseStr [] text)!!12) :: (Float,Float)
                                                        x16 = read ((parseStr [] text)!!13) :: Bool
                                                        x17 = read ((parseStr [] text)!!14) :: (Float,Float)
                                                        x18 = read ((parseStr [] text)!!15) :: Bool
                                                    return game { level = x1
                                                                , ballLoc = x2
                                                                , ballVelBuf = x3
                                                                , ballVel = x4 
                                                                , platformLoc = x5
                                                                , platformsLoc = x7
                                                                , platformSizeX = x8
                                                                , platformSizeY = x9
                                                                , gameScore = x10
                                                                , gameOverText = x11
                                                                , playerName = x12
                                                                , proFile = x13
                                                                , pastBallLoc = x15
                                                                , secret = x16
                                                                , bonusPos = x17
                                                                , bonusFlag = x18 } 

-- For an 'a' keypress
handleKeys (EventKey (Char 'a') _ _ _) game = return game { platformLoc = (x', y) }
  where 
    -- Old locations and velocities.
    (x, y) = platformLoc game

    -- New locations.
    x' = if x - 40 - (platformSizeX game)/2 >= -fromIntegral height / 2
         then x - 40
         else x

-- For an 'd' keypress
handleKeys (EventKey (Char 'd') _ _ _) game = return game { platformLoc = (x', y) }
  where 
    -- Old locations and velocities.
    (x, y) = platformLoc game

    -- New locations.
    x' = if x + 40 + (platformSizeX game)/2 <= fromIntegral height / 2
         then x + 40
         else x
handleKeys (EventKey (Char '1') _ _ _) game = return game { ballLoc = (0, (-100))
                                                          , ballVel = (0, 0)
                                                          , platformLoc = (0, (-250))
                                                          , platformsLoc = [(x,y)| x<-[-210, -100..220], y<-[90,150..290]]
                                                          , ballVelBuf = (0, 0)
                                                          , gameScore = 0
                                                          , gameOverText = " "
                                                          , pastBallLoc = (0, (-100))
                                                          , level = 1
                                                          }

handleKeys (EventKey (Char '2') _ _ _) game = return game { ballLoc = (0, (-100))
                                                          , ballVel = (0, 0)
                                                          , platformLoc = (0, (-250))
                                                          , platformsLoc = [(x,y)| x<-[-210, -150..220], y<-[90,130..250]]
                                                          , ballVelBuf = (0, 0)
                                                          , gameScore = 0
                                                          , gameOverText = " "
                                                          , pastBallLoc = (0, (-100))
                                                          , level = 2
                                                          }

handleKeys (EventKey (Char '3') _ _ _) game = return game { ballLoc = (0, (-100))
                                                          , ballVel = (0, 0)
                                                          , platformLoc = (0, (-250))
                                                          , platformsLoc = ((0,90):((-30,130): ((30,130)
                                                                           :[(x,y)| x <- [-60,0,60], y <- [170]] 
                                                                           ++[(x,y)| x <- [-210, -150..220 ], y <-[210,250]] )))
                                                          , ballVelBuf = (0, 0)
                                                          , gameScore = 0
                                                          , gameOverText = " "
                                                          , pastBallLoc = (0, (-100))
                                                          , level = 3
                                                          }

handleKeys (EventKey (Char '4') _ _ _) game = return game { level = 4 
                                                          , ballLoc = (0, (-100))
                                                          , ballVel = (0, 0)
                                                          , platformLoc = (0, (-250))
                                                          , platformsLoc = [(x,y)| x<-[-210, -160..(-50)], y<-[70,110..280]]
                                                                           ++[(x,y)| x<-[50, 100..220], y<-[70,110..280]]
                                                          , ballVelBuf = (0, 0)
                                                          , gameScore = 0
                                                          , gameOverText = " "
                                                          , pastBallLoc = (0, (-100))
                                                          
                                                          }
handleKeys (EventKey (Char '5') _ _ _) game = return game { level = 5 
                                                          , ballLoc = (0, (-100))
                                                          , ballVel = (0, 0)
                                                          , platformLoc = (0, (-250))
                                                          , platformsLoc = [(x,y)| x<-[-210, -150..220], y<-[70,110..250]]
                                                          , ballVelBuf = (0, 0)
                                                          , gameScore = 0
                                                          , gameOverText = " "
                                                          , pastBallLoc = (0, (-100))
                                                          
                                                          }
handleKeys (EventKey (Char '6') _ _ _) game = return game { level = 6 
                                                          , ballLoc = (0, (-100))
                                                          , ballVel = (0, 0)
                                                          , platformLoc = (0, (-250))
                                                          , platformsLoc = ((0,70):((-30,110): ((30,110)
                                                                           :[(x,y)| x <- [-60,0,60], y <- [150]] 
                                                                           ++[(x,y)| x <- [-210, -150..220 ], y <-[190,230,270]] )))
                                                          , ballVelBuf = (0, 0)
                                                          , gameScore = 0
                                                          , gameOverText = " "
                                                          , pastBallLoc = (0, (-100))
                                                          
                                                          }
handleKeys (EventKey (Char '7') _ _ _) game = return game { level = 7 
                                                          , ballLoc = (0, (-100))
                                                          , ballVel = (0, 0)
                                                          , platformLoc = (0, (-250))
                                                          , platformsLoc = [(x,y)| x<-[-200, -150..(-50)], y<-[70,110..280]]
                                                                           ++[(x,y)| x<-[50, 100..220], y<-[70,110..280]]
                                                                           ++[(x,y)| x<-[0], y<-[70,110]]
                                                          , ballVelBuf = (0, 0)
                                                          , gameScore = 0
                                                          , gameOverText = " "
                                                          , pastBallLoc = (0, (-100))
                                                          
                                                          }
                                                          
handleKeys (EventKey (Char 'n') Down _ _) game = return game { level = level'
                                                          , ballLoc = (0, (-100))
                                                          , ballVel = (0, 0)
                                                          , platformLoc = (0, (-250))
                                                          , platformsLoc = newLoc
                                                          , ballVelBuf = (0, 0)
                                                          , gameOverText = " "
                                                          , pastBallLoc = (0, (-100))
                                                          }
  where 
    level' =if ( mod ((level game) + 1) 8) > 0
              then 
                mod ((level game) + 1) 8
              else 
                1
    newLoc = buildPlatforms game{level = level'}

-- For an 'Esc' keypress, to exit the game.

handleKeys (EventKey (SpecialKey KeyEsc) _ _ _) game = exitSuccess

-- Do nothing for all other events.
handleKeys _ game = return game

parseStr :: String -> String -> [String]
parseStr _ [] = []
parseStr buf (x:xs) = if (x /= '%') then parseStr (buf ++ [x]) xs else buf:(parseStr [] xs)

-- Random Color
-- we don't use unsafePerformIO anymore
randColor :: PongGame -> PongGame
randColor game = case (mod (n) 4) of
                   0 -> game {gameRandomGen = newGen, randColorGame = red}
                   1 -> game {gameRandomGen = newGen, randColorGame = green}
                   2 -> game {gameRandomGen = newGen, randColorGame = blue}
                   3 -> game {gameRandomGen = newGen, randColorGame = yellow}
                 where
                   (n, newGen) = randomR range1 (gameRandomGen game)

randVel :: PongGame -> (Float,Float)
randVel game = (x',y')
  where
    y'= read (show(fst(randomR range2 (gameRandomGen game)))) :: Float
    x'= sqrt(2*250^2 - y'*y')
    
buildPlatforms ::  PongGame -> [Position]
buildPlatforms game = case (level game) of 1 -> [(x,y)| x<-[-210, -100..220], y<-[90,150..290]]

                                           2 -> [(x,y)| x<-[-210, -150..220], y<-[90,130..250]]
                                           
                                           3 -> ((0,90):((-30,130): ((30,130)
                                                :[(x,y)| x <- [-60,0,60], y <- [170]] 
                                                ++[(x,y)| x <- [-210, -150..220 ], y <-[210,250]] )))
                                                
                                           4 -> [(x,y)| x<-[-210, -160..(-50)], y<-[70,110..280]]
                                                ++[(x,y)| x<-[50, 100..220], y<-[70,110..280]]
                                                
                                           5 -> [(x,y)| x<-[-210, -150..220], y<-[70,110..250]]
                                           
                                           6 -> ((0,70):((-30,110): ((30,110)
                                                :[(x,y)| x <- [-60,0,60], y <- [150]] 
                                                ++[(x,y)| x <- [-210, -150..220 ], y <-[190,230,270]] )))
                                           7 -> [(x,y)| x<-[-200, -150..(-50)], y<-[70,110..280]]
                                                                           ++[(x,y)| x<-[50, 100..220], y<-[70,110..280]]
                                                                           ++[(x,y)| x<-[0], y<-[70,110]]
                                           
                                           _ -> [(x,y)| x<-[-210, -100..220], y<-[90,150..290]]
