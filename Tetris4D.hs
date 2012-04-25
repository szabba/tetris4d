module Main where


import Control.Monad.ST
import Data.Array.ST

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLi


-- Geometry types
type Point = (Int,[Int])
type Block = [Point]
type Well  = [Point]
type Size  = Point
type World  = (Well, Block)


-- Game logic functions

blockedBy :: Point -> Point -> Bool
(y1, r1) `blockedBy` (y2, r2) = y1 - 1 == y2 && all (uncurry (==)) (zip r1 r2)

stopsAt :: Block -> Point -> Bool
block `stopsAt` point = any (`blockedBy` point) block

canFallFurther :: Block -> Well -> Bool
block `canFallFurther` well = any (block `stopsAt`) well


-- Point arithmetics

add :: Point -> Point -> Point
(y1, r1) `add` (y2, r2) = (y1 + y2, map (uncurry (+)) (zip r1 r2))


-- All possible blocks!

zeros :: Int -> [Int]
zeros n = take n $ repeat 0

neumann :: Int -> [Point]
neumann n = (1, zeros $ n - 1) : [
  (0, zeros i ++ [1] ++ zeros (n - 2 - i))
  | i <- [0..(n-2)]] ++ [
  (0, zeros i ++ [-1] ++ zeros (n - 2 - i))
  | i <- [0..(n-2)]]

shapesFrom :: Int -> Block -> [Block]
shapesFrom n block = do
  let lastPoint = head block
  dp <- neumann n
  let new = lastPoint `add` dp
      previous = tail block
  if new `elem` previous
    then []
    else [new : block]

blockShapes' n blocks = foldl (++) [] $ map (shapesFrom n) blocks

  -- This one's quite general.
mapply :: (Integral i) => i -> (t -> t) -> t -> t
mapply 0 _ a = a
mapply n f a = mapply (n - 1) f (f a)

  -- dimmensions -> pointsInBlock -> legalBlocks
blockShapes :: Int -> Int -> [Block]
blockShapes n l = mapply (l - 1) (blockShapes' n) [[(0, zeros (n - 1))]]


-- Crossecting 
type XSection = [(Int, Int)]
type View = (XSection, XSection)
type FullView = [View]

view :: World -> (Int, Int) -> View
view world (i, j) = fmap (filterOut i j) world

filterOut :: Int -> Int -> [Point] -> [(Int, Int)]
filterOut i j points = map dropZO $ filter (zoIS i j) points
  where
    zoIS i j (y, [x, z, o]) = i == z && j == o
    dropZO (y, x:rest) = (y, x)

fullView :: (Int, Int) -> World -> FullView
fullView (zLen, oLen) world = map (view world) [(i, j) | i <- [0..(zLen-1)], j <- [0..(oLen-1)]]
  

-- Painting
-- Main function

  -- Bare in mind that this is a placeholder.
main :: IO ()
main = do
    -- option parsing will go here...
    SDL.init [SDL.InitEverything]
    -- 210 x 450 / 420 x 900 / ...
    screen <- SDL.setVideoMode 640 480 32 [SDL.SWSurface]
    SDL.setCaption "Tetris 4D" "multidimensional mayhem"
    
    blockI <- SDLi.load "block.png"
    wellI  <- SDLi.load "well.png"

    SDL.blitSurface blockI Nothing screen Nothing
    SDL.flip screen

    eventLoop
    SDL.quit
  where
    eventLoop = SDL.waitEventBlocking >>= checkEvent
    checkEvent (KeyUp _) = return ()
    checkEvent _         = eventLoop
