module GameMap where

import Control.Applicative
import Data.Array.IArray
import Data.Maybe
import Types

mapBlockToChr :: MapBlock -> Char
mapBlockToChr HeroBlock    = '@'
mapBlockToChr Wall         = 'X'
mapBlockToChr Empty        = '.'
mapBlockToChr Void         = ' '
mapBlockToChr MonsterBlock = '#'
mapBlockToChr Treasure     = 'T'

mapBlockToChr MonsterSpawn = 'S'
mapBlockToChr HeroSpawn    = '@'

chrToMapBlock :: Char -> Maybe MapBlock
chrToMapBlock c = lookup c assocList
    where assocList = map ((,) <$> mapBlockToChr <*> id) [minBound ..]

mapBlock1 :: [String]
mapBlock1 =
    ["XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    ,"X........................................X"
    ,"X....................................@...X"
    ,"X........................................X"
    ,"X........................................X"
    ,"X....XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    ,"X....X                                    "
    ,"X....X    XXXXXXXXXXX                     "
    ,"X....X    X.........X                     "
    ,"X....X    X.........X                     "
    ,"X....X    X.........X                     "
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXX "
    ,"X....XX.................................X "
    ,"X....XX.................................X "
    ,"X....XX............T....................X "
    ,"X....XX.................................X "
    ,"X....XX............T....................X "
    ,"X....XX................SSSS.............X "
    ,"X....XX................SSSS.............X "
    ,"X....XX................SSSS.............X "
    ,"X....XX.................................X "
    ,"X....XX.................................X "
    ,"X....XX.................................X "
    ,"X....XX.................................X "
    ,"X....XX.................................X "
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXX "
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXX   "
    ,"X.....................................X   "
    ,"X.....................................X   "
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXX   "
    ,"XXXXXX    XXXXXXXXXXX                     "
    ]




loadMap :: [String] -> Maybe Map
loadMap input =
    if null input || any null input then Nothing else
    let h = toInteger $ length input
        w = toInteger . length . head $ input in
    listArray ((0, 0), (h - 1, w - 1)) <$> mapM chrToMapBlock (concat input)

forceMap :: Maybe Map -> Map
forceMap = fromMaybe (error "Error loading map :(")

-- return a list of all coordinates for a given block type
findBlocks :: MapBlock -> Map -> [((Integer, Integer), MapBlock)]
findBlocks block = filter ((== block).snd) . assocs
