module Console
( defaultKeys
, draw
, getAction
) where

import Control.Lens
import Data.Array.IArray
import Graphics.Vty

import GameMap
import Types

draw :: Vty -> Game ()
draw vty world = do
    let map' = amap drawCell . addActors $ world
        (_, (h, w)) = bounds map'
        picture = pic_for_image . vert_cat $ do
            y <- [0 .. h]
            return . horiz_cat $ do
                x <- [0 .. w]
                return (map' ! (y, x))
    update vty $ picture { pic_cursor = NoCursor }
    where drawCell cell = string (blockAttr cell) [mapBlockToChr cell]

addActors :: World -> Map
addActors world = addMonsters (world^.wmonsters) $ addHero (world^.whero) $ world^.wmap
    where addHero hero = (// [(coords hero, HeroBlock)])
          addMonsters monsters  = (// zip (map coords monsters) (repeat MonsterBlock))

blockAttr :: MapBlock -> Attr
blockAttr HeroBlock    = with_fore_color def_attr bright_blue
blockAttr MonsterBlock = with_fore_color def_attr bright_red
blockAttr Treasure     = with_fore_color def_attr bright_yellow
blockAttr _            = def_attr

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
