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
        picture = pic_for_image . vert_cat $ map horiz_cat rows
    update vty $ picture { pic_cursor = NoCursor }


drawCell :: Block -> Image
drawCell Void               = string (with_fore_color def_attr bright_black) "."
drawCell Empty              = string def_attr " "
drawCell Wall               = string (with_fore_color def_attr bright_black) "X"
drawCell (ActorBlock actor) = actorImage actor


type KeyMap = [(Event, GAction)]

defaultKeys :: KeyMap
defaultKeys =
    [(EvKey KEsc [],         Quit)
    ,(EvKey (KASCII 'q') [], Quit)
    ,(EvKey KUp [],          Move DUp)
    ,(EvKey (KASCII 'w') [], Move DUp)
    ,(EvKey KDown [],        Move DDown)
    ,(EvKey (KASCII 's') [], Move DDown)
    ,(EvKey KLeft [],        Move DLeft)
    ,(EvKey (KASCII 'a') [], Move DLeft)
    ,(EvKey KRight [],       Move DRight)
    ,(EvKey (KASCII 'd') [], Move DRight)
    ]


getAction :: KeyMap -> Vty -> IO GAction
getAction keyMap vty = do
    ev <- next_event vty
    case lookup ev keyMap of
        Just action -> return action
        _           -> getAction keyMap vty
