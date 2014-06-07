module Level
  ( fillVoid
  , loadLevel
  , forceLevel
  ) where


import Data.Array.IArray
import Data.Maybe
import Types


chrToBlock :: Char -> Block
chrToBlock 'X' = Wall
chrToBlock '.' = Empty
chrToBlock _   = Void


data LevelBlock = HeroSpawn
                | MonsterSpawn
                | TreasureSpawn
                deriving (Bounded, Enum, Eq, Show)


loadLevel :: Monster m => [String] -> [(Char, Coords -> m)] -> (Level, Hero, [m])
loadLevel input mMap =
    let level = parseLevel chrToBlock input
        chrLevel = parseLevel id input
        hero = mkHero . head $ findBlocks '@' chrLevel
        monsters = concat [map f $ findBlocks c chrLevel | (c, f) <- mMap]
    in (level, hero, monsters)


parseLevel :: (a -> b) -> [[a]] -> Array Coords b
parseLevel fn input =
    let h = toInteger $ length input
        w = toInteger . length . head $ input in
    listArray ((0, 0), (h - 1, w - 1)) $ map fn (concat input)


-- return a list of all coordinates for a given block type
findBlocks :: Eq a => a -> Array Coords a -> [Coords]
findBlocks x = map fst . filter ((== x) . snd) . assocs


forceLevel :: Maybe Level -> Level
forceLevel = fromMaybe (error "Error loading map :(")


{- Find all squares that are surrounded by Void or Wall. and convert them to Voids.
 - This works by making copies of the map that are offset by +/-1 in all directions,
 - and then merging them together by appending duplicate values in a list, and then
 - checking the resulting list. Quite horrible,
 - -}
fillVoid :: Level -> Level
fillVoid level =
    let newBlock blocks = if all (`elem` [Void, Wall]) blocks then Void else head blocks
        in
    amap newBlock $ makeAdjArr level


offsetArr :: (Num a, Ix a) => Array (a, a) e -> (a, a) -> Array (a, a) e
offsetArr arr offset =
    let (b1, b2) = bounds arr in
    array (b1 |+| offset, b2 |+| offset) [(i |+| offset, arr ! i) | i <- range (b1, b2)]


growArr :: (Num a, Ix a) => Array (a, a) [e] -> Array (a, a) [e]
growArr arr =
    let (b1, b2) = bounds arr
        (b1', b2') = (b1 |-| (1, 1), b2 |+| (1, 1))
    in
    -- make a new map filled with empty lists that is one block bigger in all dimensions
    -- and then fill it with the original array
    array (b1', b2') [(i, []) | i <- range (b1', b2')] //
        [(i, arr ! i) | i <- range (b1, b2)]


shrinkArr :: (Num a, Ix a) => Array (a, a) [e] -> Array (a, a) [e]
shrinkArr arr =
    let (b1, b2) = bounds arr
        bounds' = (b1 |+| (1, 1), b2 |-| (1, 1))
    in
    array bounds' [(i, arr ! i) | i <- range bounds']


makeAdjArr :: LevelArray Block -> LevelArray [Block]
makeAdjArr level =
    let listArr = amap (:[]) level
        offsets = [(y, x) | x <- [-1, 0, 1], y <- [-1, 0, 1], x /= 0 && y/= 0]
        offsetLevels = map (offsetArr listArr) offsets
        foldStep arr seed = accum (++) seed (assocs arr)
            in
    shrinkArr $ foldr foldStep (growArr listArr) offsetLevels
