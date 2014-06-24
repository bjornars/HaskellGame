module Console
( defaultKeys
, draw
, getAction
) where

import Data.Array.IArray
import Data.Function (on)
import Data.List (groupBy)
import Graphics.Vty

import Types


draw :: Vty -> Level -> IO ()
draw vty level = do
    let level' = amap drawCell level
        cells = assocs level'
        rows = (map.map) snd $ groupBy ((==) `on` fst.fst) cells
        picture = picForImage . vertCat $ map horizCat rows
    update vty $ picture { picCursor = NoCursor }


drawCell :: Block -> Image
drawCell Void               = string (withForeColor defAttr brightBlack) "."
drawCell Empty              = string defAttr " "
drawCell Wall               = string (withForeColor defAttr brightBlack) "X"
drawCell (ActorBlock actor) = actorImage actor


type KeyMap = [(Event, GAction)]

defaultKeys :: KeyMap
defaultKeys =
    [(EvKey KEsc [],         Quit)
    ,(EvKey (KChar 'q') [], Quit)
    ,(EvKey KUp [],          Move DUp)
    ,(EvKey (KChar 'w') [], Move DUp)
    ,(EvKey KDown [],        Move DDown)
    ,(EvKey (KChar 's') [], Move DDown)
    ,(EvKey KLeft [],        Move DLeft)
    ,(EvKey (KChar 'a') [], Move DLeft)
    ,(EvKey KRight [],       Move DRight)
    ,(EvKey (KChar 'd') [], Move DRight)
    ,(EvKey (KChar ' ') [], Attack)
    ]


getAction :: KeyMap -> Vty -> IO GAction
getAction keyMap vty = do
    ev <- nextEvent vty
    case lookup ev keyMap of
        Just action -> return action
        _           -> getAction keyMap vty
