module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height :: Int
width = 600
height = 600

window :: Display
window = FullScreen

background :: Color
background = black

type Score = Int

-- | Data describing the state of the pong game. 
data PongGame = Game
  { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVelBuf :: (Float, Float) -- ^ helping buffer.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity. 
  , platformLoc :: (Float, Float) -- ^ Platform (x, y) location.
  , gameScore :: Score
  , gameOverText :: String
  } deriving Show

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (0, (-100))
  , ballVel = (0, 0)
  , platformLoc = (0, (-250))
  , ballVelBuf = (0, 0)
  , gameScore = 0
  , gameOverText = ""
  }

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game = pictures [ ball, walls, platform, gameName, authors, drawScore(gameScore game), drawGameOverText(game)]
  where
    -- The gameName.    
    gameName = translate (-650) (280) $ scale 0.5 0.5 $ color white $ text "Arkanoid!"

    -- The authors.
    authors = translate (400) (-380) $ scale 0.1 0.1 $ color white $ text "Created by KorotkovBS and RozkovNO (c)"

    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 8
    ballColor = dark red

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

    --  The platform.
    platform = uncurry translate (platformLoc game) $ color col $ rectangleSolid 90 10
    col = green

-- | Draw Score
drawScore :: Score -> Picture
drawScore score = translate (-w) (-h) (scale 30 30 (pictures
  [ color white (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- белая рамка
  , color black (polygon [ (0.1, -0.1), (0.1, -1.9), (5.9, -1.9), (5.9, -0.1) ])    -- чёрные внутренности
  , translate 2.75 (-1.5) (scale 0.01 0.01 (color red (text (show score))))  -- красный счёт
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

moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

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
wallBounce game = game { ballVel = (vx', vy'), gameScore = score' }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 8
    score = gameScore game

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

    score' = if wallCollisionY (ballLoc game) radius || wallCollisionX (ballLoc game) radius
            then 
               score + 1
            else
               score

platformCollision :: Position -> Position -> Radius -> Bool 
platformCollision (x, y) (bx, by) radius = collisionX && collisionY
  where
    collisionX  = (x + 45 >= bx + radius) && (x - 45 <= bx - radius) -- Расчёты с условием ширины платформы
    collisionY  = by - radius < (-250) -- Расчёты с условием высоты платформы

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 8

    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if platformCollision (platformLoc game) (ballLoc game) radius
          then
             -- Update the velocity.
             -vy
           else
             -- Do nothing. Return the old velocity.
             vy
    vx' = vx

checkGameOver :: PongGame -> PongGame
checkGameOver game = if snd (ballLoc game) - 8 < snd (platformLoc game) - 5 then game {ballVel = (0, 0), gameOverText = "GAME OVER!"} else game


-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> PongGame
update seconds = checkGameOver . paddleBounce . wallBounce . moveBall seconds

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 's') _ _ _) game =
  game { ballLoc = (0, -100), ballVel = (250, -250), gameScore = 0, gameOverText = "" }

-- For an 'r' keypress, to pause the game.
handleKeys (EventKey (Char 'r') _ _ _) game = game { ballVelBuf = ballVel game }

-- For an 'p' keypress, to pause the game.
handleKeys (EventKey (Char 'p') _ _ _) game = let v = ballVel game 
                                                in game { ballVelBuf = v, ballVel = (0, 0) }

-- For an 'g' keypress, to unpause the game.
handleKeys (EventKey (Char 'g') _ _ _) game = game { ballVel = ballVelBuf game }

-- For an 'a' keypress
handleKeys (EventKey (Char 'a') _ _ _) game = game { platformLoc = (x', y) }
  where 
    -- Old locations and velocities.
    (x, y) = platformLoc game

    -- New locations.
    x' = if x - 30 - 45 >= -fromIntegral height / 2
         then x - 30
         else x

-- For an 'd' keypress
handleKeys (EventKey (Char 'd') _ _ _) game = game { platformLoc = (x', y) }
  where 
    -- Old locations and velocities.
    (x, y) = platformLoc game

    -- New locations.
    x' = if x + 30 + 45 <= fromIntegral height / 2
         then x + 30
         else x

-- Do nothing for all other events.
handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update
