module GameMap where

import Control.Applicative
import Control.Monad
import Data.Array.IArray
import Data.Maybe
import Types

mapBlockToChr :: MapBlock -> Char
mapBlockToChr HeroBlock    = '@'
mapBlockToChr Wall         = 'X'
mapBlockToChr Empty        = '.'
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
    ,"X....XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    ,"X....XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXXX"
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXXX"
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXXX"
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXXX"
    ,"X....XX.................................XX"
    ,"X....XX.................................XX"
    ,"X....XX............T....................XX"
    ,"X....XX.................................XX"
    ,"X....XX............T....................XX"
    ,"X....XX................SSSS.............XX"
    ,"X....XX................SSSS.............XX"
    ,"X....XX................SSSS.............XX"
    ,"X....XX.................................XX"
    ,"X....XX.................................XX"
    ,"X....XX.................................XX"
    ,"X....XX.................................XX"
    ,"X....XX.................................XX"
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXXX"
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXXX"
    ,"X.....................................XXXX"
    ,"X.....................................XXXX"
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXXX"
    ,"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
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
