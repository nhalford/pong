{-
 - Pong in Haskell using reactive-banana for FRP
 - Most of the code is adapted from the analogous tutorial for elm at
 - http://elm-lang.org/blog/making-pong
 -}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Pong where

import Graphics.UI.WX
import Reactive.Banana
import Reactive.Banana.WX

-- basic data structures
data Ball = Ball { bx, by, bvx, bvy :: Double } -- b prefix for ball
data Player = Player { px, py, dest :: Double, score :: Int } -- p prefix for player
data GameState = Play | Pause deriving Eq
data Game = Game { state :: GameState, ball :: Ball, player1, player2 :: Player }

-- basic game data
width, height, halfWidth, halfHeight :: Int
width = 600
height = 400
halfWidth = width `div` 2
halfHeight = height `div` 2

radius :: Int -- ball radius
radius = 5

playerWidth, playerHeight :: Int -- paddle size
playerWidth = 10
playerHeight = 50

playerSize :: Size2D Int
playerSize = sz playerWidth playerHeight
        
-- colors
background, fontColor :: Color
background = colorRGB 60 100 60
fontColor = colorRGB 160 200 160

changeGameState :: GameState -> GameState
changeGameState Play = Pause
changeGameState Pause = Play

-- set destination equal to position so we don't unexpectedly move after unpausing
resetPlayer :: Player -> Player
resetPlayer p = p { dest = py p }

pauseUnpause :: Game -> Game
pauseUnpause g = g { state = changeGameState $ state g
                   , player1 = resetPlayer $ player1 g
                   , player2 = resetPlayer $ player2 g
                   }

-- player initialized at position x
player :: Double -> Player
player x = Player x middle middle 0
    where middle = fromIntegral $ halfHeight - (playerHeight `div` 2)

-- initial game state
defaultGame :: Game
defaultGame = Game Pause b p1 p2 where
    b = Ball (fromIntegral halfWidth) (fromIntegral halfHeight) 17 7
    p1 = player 10
    p2 = player $ fromIntegral $ width - playerWidth - 10

resetBall :: Game -> Game
resetBall g = g { state = Pause, ball = b' }
    where b' = defaultBall { bvx = (signum $ bvx $ ball g) * (bvx defaultBall) }
          defaultBall = ball defaultGame

-- check if ball has hit a paddle
within :: Ball -> Player -> Bool
within b p = near px' (bx b) 8 && near py' (by b) (1 + r + halfPlayerHeight)
    where py' = py p + halfPlayerHeight
          px' = px p + halfPlayerWidth
          halfPlayerHeight = fromIntegral $ playerHeight `div` 2
          halfPlayerWidth = fromIntegral $ playerWidth `div` 2
          r = fromIntegral radius
          near n m = (<=) (abs $ n - m)

-- change velocity direction
stepV :: Double -> Bool -> Bool -> Double
stepV v True _ = abs v -- upper collision
stepV v _ True = -(abs v) -- lower collision
stepV v _ _ = v

-- move a player by 1; trickery for smoother animations
updatePlayer :: Player -> Player
updatePlayer p
    | dest p < py p - 1 = p { py = max ((py p) - 1) 0 }
    | dest p > py p + 1 = p { py = min ((py p) + 1) (fromIntegral $ height - playerHeight) }
    | otherwise = p

updateGame :: Game -> Game
updateGame g
    | state g == Pause = g
    | otherwise = moveBall $ g { player1 = updatePlayer $ player1 g
                               , player2 = updatePlayer $ player2 g
                               }

data Direction = Up | Down deriving Eq -- helper for moving

ddest :: Double -- amount to move player by; essentially a velocity
ddest = 23

-- for moving player
changeDest :: Double -> Player -> Player
changeDest d p 
    | abs ((py p) - (dest p)) >= (abs d) = p
    | d >= 0 = p { dest = max 0 ((dest p) - d) }
    | otherwise = p { dest = min (fromIntegral $ height - playerHeight) ((dest p) - d) }

-- TODO: Is there a better implementation of moveP1/2? Template Haskell?
moveP1, moveP2 :: Direction -> Game -> Game
moveP1 d g
    | state g == Pause = g
    | d == Up = g { player1 = changeDest ddest (player1 g) }
    | otherwise = g { player1 = changeDest (-ddest) (player1 g) }

moveP2 d g
    | state g == Pause = g
    | d == Up = g { player2 = changeDest ddest (player2 g) }
    | otherwise = g { player2 = changeDest (-ddest) (player2 g) }

moveBall :: Game -> Game
moveBall g
    | bx' < 0 = resetBall $ g { player2 = p2 { score = 1 + score2 } }
    | bx' > (fromIntegral width) = resetBall $ g { player1 = p1 { score = 1 + score1 } }
    | otherwise = g { ball = b { bvx = stepV (bvx b) (within b p1) (within b p2)
                               , bvy = stepV (bvy b) (y < 0) (y > fromIntegral height)
                               , bx = bx'
                               , by = y + 0.105 * (bvy b)
                               }
                     }
    where b = ball g
          bx' = (bx b) + 0.105 * (bvx b)
          p1 = player1 g
          p2 = player2 g
          score1 = score p1
          score2 = score p2
          y = by b

-- adapted from paintBalls function at https://wiki.haskell.org/WxHaskell/Quick_start
drawBall :: Ball -> DC a -> Rect -> IO ()
drawBall b dc _ = do
    circle dc (pt (round $ bx b) (round $ by b)) radius [ brushColor := white
                                                        , brushKind := BrushSolid
                                                        ]

drawPlayer :: Player -> DC a -> Rect -> IO ()
drawPlayer p dc _ = do
    let prect = rect (pt (round $ px p) (round $ py p)) playerSize
    drawRect dc prect [ brushColor := white, brushKind := BrushSolid ]

drawScores :: Game -> DC a -> Rect -> IO ()
drawScores g dc _ = do
    let score1 = show $ score $ player1 g
    let score2 = show $ score $ player2 g
    let textAttrs = [ fontSize := 40, textColor := fontColor ]
    drawText dc score1 (point (width `div` 4) (height `div` 6)) textAttrs
    drawText dc score2 (point (3 * width `div` 4) (height `div` 6)) textAttrs

drawGame :: Game -> DC a -> Rect -> IO ()
drawGame g dc viewArea = do
    let g' = updateGame g
    drawBall (ball g') dc viewArea
    drawPlayer (player1 g') dc viewArea
    drawPlayer (player2 g') dc viewArea
    drawScores g dc viewArea

playGame :: IO ()
playGame = do
    let game = defaultGame

    f <- frameFixed [ bgcolor := background ]
    dt <- timer f [ interval := 5 ]
    p <- panel f [ on paint := drawGame game ]
    set f [ layout := minsize (sz width height) $ widget p ]

    let networkDescription :: MomentIO ()
        networkDescription = mdo

            etick <- event0 dt command
            ekey <- event1 p keyboard

            -- KNOWN ISSUES: Can't move both at the same time; for some reason w can't repeat
            let eup1 = filterE ((== KeyChar 'w') . keyKey) ekey
            let edown1 = filterE ((== KeyChar 's') . keyKey) ekey
            let eup2 = filterE ((== KeyUp) . keyKey) ekey
            let edown2 = filterE ((== KeyDown) . keyKey) ekey
            let espace = filterE ((== KeySpace) . keyKey) ekey

            (bgame :: Behavior Game) <- accumB game $ unions
                [ moveP1 Up <$ eup1
                , moveP1 Down <$ edown1
                , moveP2 Up <$ eup2
                , moveP2 Down <$ edown2
                , pauseUnpause <$ espace
                , updateGame <$ etick
                ]

            sink p [ on paint :== drawGame <$> bgame ]
            reactimate $ repaint p <$ etick

    network <- compile networkDescription
    actuate network
