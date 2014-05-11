module Console
( defaultKeys
, draw
, getAction
) where

import Graphics.Vty

import GameMap
import Types

draw :: Vty -> Game ()
draw vty world = do
    let picture = pic_for_image . drawMap . _wmap $ world
    update vty $ picture { pic_cursor = NoCursor }
    where drawMap = foldr1 (<->) . map drawRow
          drawRow = foldr1 (<|>) . map drawCell
          drawCell cell = string (blockAttr cell) (mapBlockToChr cell : [])

blockAttr :: MapBlock -> Attr
blockAttr HeroSpawn    = with_fore_color def_attr bright_blue
blockAttr Wall         = def_attr
blockAttr Empty        = def_attr
blockAttr MonsterSpawn = def_attr
blockAttr Monster      = def_attr
blockAttr Treasure     = def_attr

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
