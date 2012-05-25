module Main where


import System.Random

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLi


import Logic


-- Event handling {{{1

    -- Types
data Handler = Handler (HandlerFunc)

type HandlerFunc = SDL.Event -> [Handler] -> IO [Handler]

eventLoop :: [Handler] -> IO ()
eventLoop []     = return ()
eventLoop (Handler h:hs) = do
    event <- pollEvent
    newHs <- h event hs
    eventLoop newHs

    -- HandlerFuncs
exit :: HandlerFunc
exit _ _ = return []

menu :: [String] -> HandlerFunc
menu opts (KeyDown key) [] = do
    if SDL.symKey key == SDL.SDLK_SPACE
      then do
        putStrLn $ head opts
        return [Handler $ menu (tail opts)]
      else return [Handler $ menu opts]
menu opts _             [] = do
    return [Handler $ menu opts]



-- Main function {{{1

main :: IO ()
main = do
    -- option parsing will go here...
    SDL.init [SDL.InitEverything]
    screen <- SDL.setVideoMode 800 600 32 [SDL.SWSurface]
    SDL.setCaption "Tetris 4D" "multidimensional mayhem"

    blockI <- SDLi.load "block.png"
    wellI  <- SDLi.load "well.png"

    SDL.blitSurface blockI Nothing screen Nothing
    SDL.flip screen

    eventLoop [Handler $ menu $ cycle ["One", "Two", "Three"]]
