module GameMap where

import Control.Monad
import Types

mapBlockToChr :: MapBlock -> Char
mapBlockToChr HeroSpawn    = '@'
mapBlockToChr Wall         = 'X'
mapBlockToChr Empty        = '.'
mapBlockToChr MonsterSpawn = 'S'
mapBlockToChr Monster      = '#'
mapBlockToChr Treasure     = 'T'

chrToMapBlock :: Char -> Maybe MapBlock
chrToMapBlock c = lookup c assocList
    where assocList = map (liftM2 (,) mapBlockToChr id) [minBound ..]

mapBlock1 :: Maybe Map
mapBlock1 = loadMap [
     "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
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
    ,"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" ]




loadMap :: [String] -> Maybe Map
loadMap = sequence.fmap (sequence.fmap chrToMapBlock)

forceMap :: Maybe Map -> Map
forceMap (Just m) = m
forceMap _ = error "Error loading map :("

-- return a list of all coordinates for a given block type
findBlocks :: MapBlock -> Map -> [(Integer, Integer)]
findBlocks block = map fst . filter ((== block).snd) . indexBlocks

-- convert [[a]] to a list of coordinate-mapblock tuples
indexBlocks :: Map -> [((Integer, Integer), MapBlock)]
indexBlocks m = concatMap combine indexes
    where rows = map (zip [0..]) m
          indexes = zip [0..] rows
          combine (y, xs) = map (\(x,val) -> ((x, y), val)) xs
