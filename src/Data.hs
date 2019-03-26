module Data where

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

width, height :: Int
width = 600
height = 600

window :: Display
window = FullScreen

background :: Color
background = black

type Score = Int

range1 :: (Int, Int)
range1 = (0, 100)

range2 :: (Int, Int)
range2 = (-250, -150)

type Radius = Float 
type Position = (Float, Float)

-- | Data describing the state of the pong game. 
data PongGame = Game
  { level :: Integer
  , ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVelBuf :: (Float, Float) -- ^ helping buffer.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity. 
  , platformLoc :: (Float, Float) -- ^ Platform (x, y) location.
  , platformColor :: Color
  , platformsLoc :: [(Float,Float)]
  , platformSizeX :: Float
  , platformSizeY :: Float
  , gameScore :: Score
  , gameOverText :: String
  , playerName :: String
  , proFile :: String
  , gameState :: Integer -- data GameState = GameStatePlay | GameStateBonus ...
  , pastBallLoc :: (Float,Float)
  , secret :: Bool
  , bonusPos :: (Float,Float)
  , bonusFlag :: Bool
  , gameRandomGen :: StdGen
  , randColorGame :: Color
  } deriving Show

-- | The starting state for the game of Pong.
initialState :: StdGen -> String -> String -> PongGame
initialState gen prof name = Game
  { level = 1
  , ballLoc = (0, (-100))
  , ballVel = (0, 0)
  , platformLoc = (0, (-250))
  , platformColor = green
  , platformsLoc = [(x,y)| x<-[-210, -100..220], y<-[90,150..290]]
  , platformSizeX = 90
  , platformSizeY = 10
  , ballVelBuf = (0, 0)
  , gameScore = 0
  , gameOverText = " "
  , playerName = name
  , proFile = prof
  , gameState = 0
  , pastBallLoc = (0, (-100))
  , secret = False
  , bonusPos = (0,250)
  , bonusFlag = False
  , gameRandomGen = gen
  , randColorGame = orange
  }
