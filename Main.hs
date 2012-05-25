module Main where


import System.Random

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLi


import Logic
import Events


-- Main function

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

    handlerFuncS <- one $ menu [("One", dropTop), ("Exit", exit)]
    eventLoop handlerFuncS
