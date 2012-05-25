module Events where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLi

-- Types {{{1
data Handler = Handler (HandlerFunc)

type HandlerFunc = SDL.Event -> [Handler] -> IO [Handler]

-- Event loop {{{1
eventLoop :: [Handler] -> IO ()
eventLoop []     = return ()
eventLoop hs@(Handler h:_) = do
    event <- pollEvent
    newHs <- h event hs
    eventLoop newHs

-- Helpers {{{1
one :: HandlerFunc -> IO [Handler]
one hf = return [Handler hf]

push :: HandlerFunc -> [Handler] -> IO [Handler]
push hf hs = return (Handler hf : hs)

drop' :: Int -> [Handler] -> IO [Handler]
drop' n = return . drop n

replaceTop :: HandlerFunc -> [Handler] -> IO [Handler]
replaceTop hf (h:hs) = return $ Handler hf : hs

-- HandlerFuncS {{{1
exit :: HandlerFunc
exit _ _ = return []

    -- The name is misleading -- it will actually block the program!
doNothing :: HandlerFunc
doNothing _ hs = return hs

    -- While this one will go back to it's pusher.
dropTop :: HandlerFunc
dropTop event hs = do
    pushEvent event
    drop' 1 hs

menu :: [(String, HandlerFunc)] -> HandlerFunc
menu opts (KeyDown key) hs = act key'
  where
    key' = symKey key

    (_, hf) = head opts
    (name, _) = opts !! 1
    rest = (drop 1 . cycle) opts

    act SDL.SDLK_SPACE  = do
        putStrLn $ name
        one $ menu rest

    act SDL.SDLK_RETURN = push hf hs
    act _               = one $ menu rest
        
menu opts _             _ = one $ menu opts

