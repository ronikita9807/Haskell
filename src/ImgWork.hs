module ImgWork where

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
--import Text.Read hiding (Char)
import Data.Time
import Text.Read (readMaybe)

import Control.DeepSeq
import qualified System.IO as SIO

-- | Изображения объектов.
data Images = Images
  { imageKarina  :: Picture   -- ^ Изображение Карины.
  , imageArkanoid   :: Picture
  , imageSmile   :: Picture
  , imageCat1 :: Picture
  , imageCat2 :: Picture
  , imageCat3 :: Picture
  , imageBonusDoge :: Picture
  , imageBonusDogeHelp :: Picture
  , imageDoge :: Picture
  , imageRocket :: Picture
  }

-- | Загрузить изображения из файлов.
loadImages :: IO Images
loadImages = do
  Just karina   <- loadJuicyPNG "images/karina.png"
  Just arkanoid   <- loadJuicyPNG "images/arkanoid.png"
  Just smile   <- loadJuicyPNG "images/smile.png"
  Just cat1   <- loadJuicyPNG "images/cat1.png"
  Just cat2   <- loadJuicyPNG "images/cat2.png"
  Just cat3   <- loadJuicyPNG "images/cat3.png"
  Just bonusDoge <- loadJuicyPNG "images/doge.png"
  Just doge <- loadJuicyPNG "images/dogebread.png"
  Just rocket <- loadJuicyPNG "images/rocket.png"
  return Images
    { imageKarina   = scale 1.0 1.0 karina
    , imageArkanoid    = scale 1.0 1.0 arkanoid
    , imageSmile    = scale 0.2 0.2 smile
    , imageCat1 = scale 1.0 1.0 cat1
    , imageCat2 = scale 1.0 1.0 cat2
    , imageCat3 = scale 1.0 1.0 cat3
    , imageBonusDoge = scale 0.1 0.1 bonusDoge
    , imageBonusDogeHelp = scale 0.13 0.13 bonusDoge
    , imageDoge = scale 1.0 1.0 doge
    , imageRocket = scale 0.1 0.1 rocket
    }
