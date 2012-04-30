module Main where


import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLi


-- Geometry types
type Point = [Int]
type Block = [Point]
type Well  = [Point]
type World = (Well, Block)


-- Game logic functions

blockedBy :: Point -> Point -> Bool
p1 `blockedBy` p2 = y1 - 1 == y2 && r1 == r2
  where
    y1 = head p1
    y2 = head p2
    r1 = tail p1
    r2 = tail p2

stopsAt :: Block -> Point -> Bool
block `stopsAt` point = any (`blockedBy` point) block

canFallFurther :: Block -> Well -> Bool
block `canFallFurther` well = any (block `stopsAt`) well


-- Translation

add :: Point -> Point -> Point
p1 `add` p2 = zipWith (+) p1 p2

inverse :: Point -> Point
inverse = map ((-1) *)
 
-- Rotation

type Axis = Int
type Around = (Point, [Axis])
data RotationDir = Clockwise | Counter

  -- Beware: if the list of fixed axes is shorter than the length of a point
  -- minus one, it will behave in a funny way.
left :: [Axis] -> Point -> Point

left fixedAxes r = map rotatedAt indices
  where
    rotatedAt :: Int -> Int
    rotatedAt i
        | freeAxes !! 0 == i = - r !! (freeAxes !! 1)
        | freeAxes !! 1 == i =   r !! (freeAxes !! 0)
        | otherwise          =   r !! i

    freeAxes :: [Axis]
    freeAxes = filter (not . (`elem` fixedAxes)) indices

    indices = [0..lenR - 1]

    lenR = length r

right :: [Axis] -> Point -> Point
right fixedAxes = mapply 3 (left fixedAxes)

rotatePoint :: RotationDir -> [Axis] -> Point -> Point
rotatePoint Clockwise = left
rotatePoint Counter   = right

rotate :: RotationDir -> [Axis] -> Block -> Block
rotate rotDir fixedAxes = map $ rotatePoint rotDir fixedAxes
  where
    center :: Block -> Point
    center = map average . translocate

    average :: [Int] -> Int
    average iS = sum iS `div` length iS

    translocate :: [[a]] -> [[a]]
    translocate = map reverse . foldl foldMe (repeat [])
      where
        foldMe = zipWith $ Prelude.flip (:)


-- Valid blocks generation

zeros :: Int -> [Int]
zeros n = take n $ repeat 0

neumann :: Int -> [Point]
neumann n = [
  zeros i ++ [ 1] ++ zeros (n - 1 - i) | i <- [0..(n-1)]] ++ [
  zeros i ++ [-1] ++ zeros (n - 1 - i) | i <- [0..(n-1)]]

  -- This one's quite general.
mapply :: (Integral i) => i -> (t -> t) -> t -> t
mapply 0 _ a = a
mapply n f a = mapply (n - 1) f (f a)

  -- dimmensions -> pointCount -> legalBlocks
blockShapes :: Int -> Int -> [Block]
blockShapes n l = map reverse $ mapply (l - 1) (blockShapes' n) [[zeros n]]
  where
    blockShapes' :: Int -> [Block] -> [Block]
    blockShapes' n blocks = foldl (++) [] $ map (shapesFrom n) blocks

    shapesFrom :: Int -> Block -> [Block]
    shapesFrom n block = do
      let lastPoint = head block
      dp <- neumann n
      let new = lastPoint `add` dp
          previous = tail block
      if new `elem` previous
        then []
        else [new : block]


-- Crossecting 
--
-- Beware as this part is specifically 4d while the code before was
-- n-dimmensional.

type XYPoint  = (Int, Int)
type XYSection = [(Int, Int)]
type View     = (XYSection, XYSection)
type FullView = [View]

filterOut :: Int -> Int -> [Point] -> [(Int, Int)]
filterOut i j points = map dropZO $ filter (zoIs i j) points
  where
    zoIs :: Int -> Int -> Point -> Bool
    zoIs i j [y, x, z, o] = i == z && j == o

    dropZO :: Point -> (Int, Int)
    dropZO (y:x:_) = (y, x)

view :: World -> (Int, Int) -> View
view (well, block) (i, j) = (filterOut i j well, filterOut i j block)

fullView :: (Int, Int) -> World -> FullView
fullView (zLen, oLen) world = map (view world) [(i, j)
    | i <- [0..(zLen-1)], j <- [0..(oLen-1)]]
   

-- Painting
-- Event handling

eventLoop :: IO ()
eventLoop = SDL.pollEvent >>= handle

handle :: SDL.Event -> IO ()
handle (KeyUp _) = SDL.quit >> return ()
handle _         = eventLoop


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
