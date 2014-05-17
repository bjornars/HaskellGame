module Level (
    level1
  , fillVoid
  , loadLevel
  , forceLevel
  , findBlocks
  , blockToChr
) where


import Control.Applicative
import Data.Array.IArray
import Data.Maybe
import Types

blockToChr :: Block -> Char
blockToChr HeroBlock    = '@'
blockToChr Wall         = 'X'
blockToChr Empty        = '.'
blockToChr Void         = ' '
blockToChr MonsterBlock = '#'
blockToChr Treasure     = 'T'

blockToChr MonsterSpawn = 'S'
blockToChr HeroSpawn    = '@'

chrToBlock :: Char -> Maybe Block
chrToBlock c = lookup c assocList
    where assocList = map ((,) <$> blockToChr <*> id) [minBound ..]

level1 :: [String]
level1 =
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
    ,"X....XX............T.XXXXXXXXXXXXXXXXXXXX "
    ,"X....XX..............XXXXXXXXXXXXXXXXXXXX "
    ,"X....XX............T.XXXXXXXXXXXXXXXXXXXX "
    ,"X....XX................SSSS.............X "
    ,"X....XX......XXXXX.....SSSS.............X "
    ,"X....XX......XXXXX.....SSSS.............X "
    ,"X....XX......XXXXX......................X "
    ,"X....XX.................................X "
    ,"X....XXXXXX.............................X "
    ,"X....XXXXXX.............................X "
    ,"X....XXXXXX.............................X "
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXXXX "
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXX   "
    ,"X.....................................X   "
    ,"X.....................................X   "
    ,"X....XXXXXX.........XXXXXXXXXXXXXXXXXXX   "
    ,"XXXXXX    XXXXXXXXXXX                     "
    ]


loadLevel :: [String] -> Maybe Level
loadLevel input =
    if null input || any null input then Nothing else
    let h = toInteger $ length input
        w = toInteger . length . head $ input in
    listArray ((0, 0), (h - 1, w - 1)) <$> mapM chrToBlock (concat input)

forceLevel :: Maybe Level -> Level
forceLevel = fromMaybe (error "Error loading map :(")

fillVoid :: Level -> Level
fillVoid m =
    let neighbormap = makeNeighborList m
        newBlock blocks = if all (`elem` [Void, Wall]) blocks then Void else head blocks
        in
    amap newBlock neighbormap


offsetArr :: (Num a, Ix a) => Array (a, a) e -> (a, a) -> Array (a, a) e
offsetArr arr offset =
    let (b1, b2) = bounds arr in
    array (b1 |+| offset, b2 |+| offset) [(i |+| offset, arr ! i) | i <- range (b1, b2)]


growArr, shrinkArr :: (Num a, Ix a) => Array (a, a) [e] -> Array (a, a) [e]
growArr arr =
    let (b1, b2) = bounds arr
        (b1', b2') = (b1 |-| (1, 1), b2 |+| (1, 1))
    in
    -- make a new map filled with empty lists that is one block bigger in all dimensions
    -- and then fill it with the original array
    array (b1', b2') [(i, []) | i <- range (b1', b2')] //
        [(i, arr ! i) | i <- range (b1, b2)]


shrinkArr arr =
    let (b1, b2) = bounds arr
        (b1', b2') = (b1 |+| (1, 1), b2 |-| (1, 1))
    in
    array (b1', b2') [(i, arr ! i) | i <- range (b1', b2')]


makeNeighborList :: LevelArray Block -> LevelArray [Block]
makeNeighborList map' =
    let listMap = amap (:[]) map'
        offsets = [(y, x) | x <- [-1, 0, 1], y <- [-1, 0, 1], x /= 0 && y/= 0]
        offsetMaps = map (offsetArr listMap) offsets
        combinedMap = shrinkArr $ foldr (\omap lmap -> accum (++) lmap (assocs omap)) (growArr listMap) offsetMaps
            in
    combinedMap


-- return a list of all coordinates for a given block type
findBlocks :: Block -> Level -> [((Integer, Integer), Block)]
findBlocks block = filter ((== block).snd) . assocs
