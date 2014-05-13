module GameMap where

import Control.Monad
import Data.Array.IArray
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
    where assocList = map (liftM2 (,) mapBlockToChr id) [minBound ..]

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
loadMap input = do
    guard $ not .null $ input
    let h = toInteger $ length input
        w = toInteger . length . (!! 0) $ input
    input' <- mapM chrToMapBlock . concat $ input
    return $ listArray ((0, 0), (h - 1, w - 1)) input'

forceMap :: Maybe Map -> Map
forceMap (Just m) = m
forceMap _ = error "Error loading map :("

-- return a list of all coordinates for a given block type
findBlocks :: MapBlock -> Map -> [((Integer, Integer), MapBlock)]
findBlocks block = filter ((== block).snd) . assocs
