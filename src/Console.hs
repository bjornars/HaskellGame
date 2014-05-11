module Console
( defaultKeys
, draw
, getAction
) where

import Control.Lens
import Graphics.Vty

import Types

draw :: Vty -> World -> IO ()
draw vty world = do
    let hero = _whero world
        heroAttr = with_fore_color def_attr bright_blue
        xPos     = fromInteger $ hero^.hxpos
        yPos     = fromInteger $ hero^.hypos
        heroImg  = translate (yPos, xPos) $ string heroAttr "@"
        picture  = pic_for_image heroImg
    update vty $ picture { pic_cursor = NoCursor }

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
