module MyProj where

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
import Text.Read (readMaybe)

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


-- | Отобразить картинку.
drawPicture :: Picture -> Float -> Float -> Float -> Picture
drawPicture image x y r = translate (x) (y) (scale r r image)

-- | Отобразить картинку.
drawBall :: Picture -> PongGame -> Picture
drawBall image game = uncurry translate (ballLoc game) (scale 0.2 0.2 image)

-- | Convert a game state into a picture.
-- TODO: remove the scary line!
render :: Images -> PongGame  -- ^ The game state to render.
       -> IO Picture   -- ^ A picture of this game state.
render images game = do  t1 <- readFile ("p1.txt")
                         t2 <- readFile ("p2.txt")
                         t3 <- readFile ("p3.txt")
                         t4 <- readFile ("p4.txt")
                         case (gameState game) of 
                           0 -> return (pictures [ secretPos
                                                 , bonPos
                                                 , ball
                                                 , walls
                                                 , platform
                                                 , platforms
                                                 , gameName
                                                 , authors
                                                 , drawScore(gameScore game)
                                                 , drawGameOverText(game)
                                                 , helloStr(playerName game)
                                                 , records 180.0 t1
                                                 , records 150.0 t2
                                                 , records 120.0 t3
                                                 , records 90.0 t4
                                                 , board 180.0
                                                 , board 150.0
                                                 , board 120.0
                                                 , board 90.0
                                                 , translate 500 200 $ color white $ rectangleSolid 250 250
                                                 , drawPicture  (imageCat1  images) (505) (200) 1.0
                                                 , drawPicture  (imageCat3  images) (-535) (-235) 0.5
                                                 , secretBonus (imageDoge images) game ])
                           1 -> return (pictures [ myText (-500) 250 0.5 0.5 white "Bonuses:"
                                                 , myText (-500) (-100) 0.5 0.5 white "Control Keys:"
                                                 , myText (-500) (-140) 0.15 0.15 white "-- Press 's' to reset the game."
                                                 , myText (-500) (-170) 0.15 0.15 white "-- Press 'p' to pause the game."
                                                 , myText (-500) (-200) 0.15 0.15 white "-- Press 'g' to resume the game."
                                                 , myText (-500) (-230) 0.15 0.15 white "-- Press 'r' to save your score in the records table."
                                                 , myText (-500) (-260) 0.15 0.15 white "-- Press 'c' to clear the records table."
                                                 , myText (-500) (-290) 0.15 0.15 white "-- Press 'o' to save the game in your profile."
                                                 , myText (-500) (-320) 0.15 0.15 white "-- Press ' l ' to load the game from your profile."
                                                 , myText (-500) (-350) 0.15 0.15 white "-- Press ' k ' to change the platform`s color."
                                                 , myText (250) (-350) 0.15 0.15 white "-- To continue the game press ' y '."
                                                 , translate (-440) (215) $ color white $ circleSolid 15
                                                 , translate (-440) (170) (imageBonusDogeHelp images)
                                                 , translate (-440) (125) $ color green $ rectangleSolid 50 10
                                                 , translate (-440) (80) $ color red $ rectangleSolid 75 10
                                                 , myText (-400) (210) 0.15 0.15 white "-- Reach it to get +3 points!"
                                                 , myText (-400) (165) 0.15 0.15 white "-- Reach it to view a SECRET bonus!"
                                                 , myText (-400) (120) 0.15 0.15 white "-- After increasing your platform, you can score 10 points, after which it will decrease again!"
                                                 , myText (-375) (75) 0.15 0.15 white "-- Every 20 points we increase your platform size!"
                                                 , drawPicture  (imageKarina  images) 550 (-200) 0.7
                                                 , drawPicture  (imageArkanoid  images) 350 (260) 0.8
                                                 , drawPicture  (imageCat2  images) 30 (-100) 0.5])
  where
    -- Hello string!
    helloStr :: String -> Picture
    helloStr name = translate (-650) (220) $ scale 0.2 0.2 $ color yellow $ text ("Welcome " ++ name ++ " !")

    -- Text Function.
    myText :: Float -> Float -> Float -> Float -> Color -> String -> Picture
    myText x y sx sy col txt = translate (x) (y) $ scale sx sy $ color col $ text txt

    -- Secret Bonus.
    secretBonus :: Picture -> PongGame -> Picture
    secretBonus image game = if (secret game) then translate (500) (-250) (scale 0.25 0.25 image)
                                              else translate (1000) (1000) $ color white $ circleSolid 5
    
    secretPos = uncurry translate (0,190)  $ color ballColor $ imageBonus1
    ballColor = if (level game ) /= 7 || (secret game)
                  then black
                  else dark red
    imageBonus1 = if (level game ) /= 7 || (secret game)
                  then circleSolid 8
                  else (imageBonusDoge images)
    
    bonPos = uncurry translate (bonusPos game)  $ color ballColors $ imageBonus2
    ballColors = if (level game ) /= 4 
                  then black
                  else white
    imageBonus2 = if (level game ) /= 4 || (secret game)
                  then circleSolid 8
                  else (imageRocket images)
    
    -- Records Table.
    records :: Float -> String -> Picture
    records y str = translate (-650) (y) $ scale 0.2 0.2 $ color green $ text str

    -- Right Records Table Board.
    board :: Float -> Picture
    board y = translate (-370) (y) $ scale 0.2 0.2 $ color green $ text "|"

    -- The gameName.    
    gameName = translate (-650) (280) $ scale 0.5 0.5 $ color white $ text "Arkanoid!"

    -- The authors.
    authors = translate (400) (-380) $ scale 0.1 0.1 $ color white $ text "Created by KorotkovBS and RozkovNO (c)"

    --  The pong ball.
    ball = drawBall  (imageSmile  images) game
    --ballColor = dark red

    --  The wallOne.
    wallOne :: Color -> Float -> Float -> Picture
    wallOne col x y =
      translate x y $
        color col $
          rectangleSolid 10 602

    -- The wallTwo.
    wallTwo :: Color -> Float -> Float -> Picture
    wallTwo col x y =
      translate x y $
        color col $
          rectangleSolid 602 10

    wallColor = light (light blue)
    walls = pictures [ wallOne wallColor (-300) 0, wallOne red 300 0, wallTwo yellow 0 300, wallTwo white 0 (-300) ]
    
    
-- *******************************************************************************************************************************
    platforms = pictures (getPlatforms getCoords1)

    getCoords1 :: [(Float,Float)]
    getCoords1 = (platformsLoc game)  
    getPlatforms :: [(Float,Float)]-> [Picture] 
    getPlatforms [] = []
    getPlatforms ((x,y):xs) = (translate x y $ color  (randColorGame game) $ rectangleSolid 40 10): getPlatforms xs
 -- ***********************************************************************************************************************************


    --  The platform.
    platform = uncurry translate (platformLoc game) $ color (platformColor game) $ rectangleSolid (platformSizeX game) (platformSizeY game)

-- | Draw Score
drawScore :: Score -> Picture
drawScore score = translate (-w) (-h) (scale 30 30 (pictures
  [ color white (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- белая рамка
  , color black (polygon [ (0.1, -0.1), (0.1, -1.9), (5.9, -1.9), (5.9, -0.1) ])    -- чёрные внутренности
  , translate 2.25 (-1.5) (scale 0.01 0.01 (color red (text (show score))))  -- красный счёт
  ]))
  where
    w = fromIntegral 1300 / 2
    h = fromIntegral 625 / 2

drawGameOverText :: PongGame -> Picture
drawGameOverText game = translate (-280) (-50) $ scale 0.7 0.7 $ color red $ text (gameOverText game)

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position

moveBall seconds game = game { pastBallLoc = ballLoc game , ballLoc = (x', y'), bonusPos=(dx',dy'), bonusFlag = flag }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game
    (dx,dy)=(bonusPos game)
    eps=150*seconds
    flag = if dy <= 50 + eps && dy >= 50 - eps
             then True
             else 
               if dy<=250 + eps && dy >= 250 - eps
                 then False 
                 else (bonusFlag game)
    dy' = if (bonusFlag game)
            then dy+eps
            else dy-eps
    dx'=if (bonusFlag game)
          then -sqrt(10000 - (dy'-150)*(dy'-150) )
          else sqrt(10000 - (dy'-150)*(dy'-150) )
    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

-- | Number of frames to show per second.
fps :: Int
fps = 60

type Radius = Float 
type Position = (Float, Float)

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollisionY :: Position -> Radius -> Bool 
wallCollisionY (x, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral width / 2 
    bottomCollision = y + radius >=  fromIntegral width / 2

wallCollisionX :: Position -> Radius -> Bool 
wallCollisionX (x, y) radius = leftCollision || rightCollision
  where
    leftCollision   = x + radius >= fromIntegral height / 2
    rightCollision  = x - radius <= -fromIntegral height / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx', vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 8
    

    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if wallCollisionY (ballLoc game) radius
          then
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy
    vx' = if wallCollisionX (ballLoc game) radius
          then
             -vx
          else
             vx

bonusCollision  :: Position -> Position -> Radius -> Bool 
bonusCollision (x, y) (bx, by) radius = collisionX && collisionY
  where
    collisionX  = abs(bx - x) <= (radius)
    collisionY  = abs(by - y) <= (radius)

secretBonusCollision  :: Position -> Position -> Radius -> Bool 
secretBonusCollision (x, y) (bx, by) radius = collisionX && collisionY
  where
    collisionX  = abs(bx - x) <= (2*radius)
    collisionY  = abs(by - y) <= (2*radius)

platformCollision :: Position -> Position -> Float -> Radius -> Bool 
platformCollision (x, y) (bx, by)  a  radius = collisionX && collisionY
  where
    collisionX  = (x + a/2 >= bx - radius) && (x - a/2 <= bx + radius) -- Расчёты с условием ширины платформы
    collisionY  = by - radius < (-250) -- Расчёты с условием высоты платформы

platformsCollision :: [Position] -> Position -> Radius -> Bool 
platformsCollision [] _ _ = False
platformsCollision ((x,y):xs) (bx, by)  radius = (collisionsX && collisionsY) || (platformsCollision xs (bx,by) radius)
  where
    collisionsX  = abs(bx - x) <= (20 + radius) -- Расчёты с условием ширины платформы
    collisionsY  = abs(by - y) <= (5 + radius) -- Расчёты с условием высоты платформы

isPlatformsCollisionY :: [Position] -> Position -> Radius -> Bool 
isPlatformsCollisionY [] _ _ = False
isPlatformsCollisionY ((x,y):xs) (px,py) radius = collisionsY || isPlatformsCollisionY xs (px,py) radius
  where 
    collisionsY = abs(py - y) > (5 + radius)


deletePlat :: [Position] -> Position -> Radius -> [Position]
deletePlat [] _ _ =[]
deletePlat ((x,y):xs) (bx,by) radius = if (abs(bx - x) <= (20 + radius) &&  abs(by - y) <= (5 + radius)) 
                                         then
                                           (deletePlat xs (bx,by) radius) 
                                         else
                                           ((x,y) : (deletePlat xs (bx,by) radius))

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy'), platformsLoc = newPlatLoc, 
                                            gameScore = score', gameOverText = text, secret= secret', platformSizeX = size, platformColor = color}
  where
    -- Radius. Use the same thing as in `render`.
    radius = 8
    score = gameScore game
    -- The old velocities.
    
    (vx, vy) = ballVel game
    
    color = if platformSizeX game == 110
              then red
              else green
    
    size = if ((mod (gameScore game) 20 ) == 0 && (gameScore game) /= 0) && (platformSizeX game) < 110
             then 
               (platformSizeX game) + 20
             else 
               if (mod (gameScore game) 20 <= 10) || gameScore game < 20 || (platformSizeX game) == 90
                 then 
                   (platformSizeX game)
                 else 
                   (platformSizeX game) - 20
    
    text = if ((platformsLoc game) == []) 
             then 
               if ((level  game) == 7)
                 then " YOU WIN! "
                 else " Press 'n' "
             else 
               " "
    secret' = if (secretBonusCollision (0,190) (ballLoc game) radius) && (level game) == 7 || secret game 
                then True
                else False
    newPlatLoc = (deletePlat (platformsLoc game) (ballLoc game) radius )
    
    
    score' = if (platformsCollision (platformsLoc game) (ballLoc game) radius)
            then 
               score + 1
            else
              if (bonusCollision (bonusPos game) (ballLoc game) radius) && (level game) == 4 
               then 
                 score +3
               else 
                 score

    
    vy' = if (platformCollision (platformLoc game) (ballLoc game) (platformSizeX game) radius) || ((isPlatformsCollisionY (platformsLoc game) ( pastBallLoc game) radius  ) && (platformsCollision (platformsLoc game) (ballLoc game) radius))
          then
             -- Update the velocity.
             -vy
           else
             -- Do nothing. Return the old velocity.
             if (platformsLoc game) == []
               then 0
               else vy
        
    vx'= if (platformsLoc game) == []
           then 0
           else vx

checkGameOver :: PongGame -> PongGame
checkGameOver game = if snd (ballLoc game) - 8 < snd (platformLoc game) - 5 then game {ballVel = (0, 0),  gameOverText = "GAME OVER!", platformSizeX = 90, platformColor = green} else game


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

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> IO PongGame
update seconds = return . checkGameOver . paddleBounce . wallBounce . moveBall seconds

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
handleKeys (EventKey (Char 'o') Down _ _) game = do writeFile ("saves/save"++ proFile game ++ ".txt") (show(level game) ++ "%" ++ show(ballLoc game)++ "%" ++ show(ballVelBuf game) ++ "%" ++ show(ballVel game) ++ "%" ++ show(platformLoc game) ++ "%" ++ show(platformsLoc game) ++ "%" ++ show(platformSizeX game) ++ "%" ++ show(platformSizeY game) ++ "%" ++ show(gameScore game) ++ "%" ++ (gameOverText game) ++ "%" ++ (playerName game) ++ "%" ++ (proFile game) ++ "%" ++ show(pastBallLoc game) ++ "%" ++ show(secret game) ++ "%" ++ show(bonusPos game) ++ "%" ++ show(bonusFlag game) ++ "%")
                                                    return game

-- For an 'l' keypress, to load the game.
handleKeys (EventKey (Char 'l') Down _ _) game = do text <- readFile ("saves/save"++ proFile game ++ ".txt")
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
    
    

runMyProj :: StdGen -> String -> String -> Images -> IO ()
runMyProj gen name prof images = playIO window background fps (initialState gen prof name) (render images) handleKeys update
