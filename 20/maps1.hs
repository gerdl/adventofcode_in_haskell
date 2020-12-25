import System.IO( openFile, hClose, hGetContents )
import System.IO( IOMode( ReadMode ) )
import Data.List.Split (splitOn, splitOneOf)
import Data.Char (isNumber, ord)
import Data.List (transpose)

-- switch between trace logging and no verbose output
--trace _ fn = fn 
import Debug.Trace (trace)


main = do  
    handle <- openFile "20/demo_1.in.txt" ReadMode
    contents <- hGetContents handle
    
    let tileset = readMultipleTiles contents

    print $ tileset

    printTile (tileset !! 0)

    let ptile = tile2PuzzleTile (tileset !! 0)

    print ptile

    hClose handle


data Tile = Tile {
    num :: Int,
    image :: [[Char]]
    }
    deriving (Eq, Read)
instance Show Tile where
    show t = "Tile-" ++ show (num t)
printTile tile = do
    putStrLn ("Tile: " ++ show (num tile))
    putStrLn (unlines (image tile))
-- printTileLine line = putStrLn line


toint :: String -> Int
toint x = read x :: Int

readTile :: [Char] -> Tile
readTile inp 
    | length data_lines == 10     = Tile myid data_lines
    | otherwise                   = error("Unable to parse Tile " ++ show inp)
    where
        firstsplit = splitOn "\n" inp
        tileid_str = (splitOneOf " :" (firstsplit!!0)) !! 1
        myid = read tileid_str :: Int
        data_lines = [ l | l <- (tail firstsplit),
                           length l == 10]

-- readMultipleTiles :: [Char] -> [Tile]
readMultipleTiles inp = tilelist
    where
        firstsplit = splitOn "Tile" inp
        tilelist = [readTile ("Tile" ++ ttext) | ttext <- firstsplit,
                                                 length ttext > 0]

tileOnTopOf ta tb = lastline_a == firstline_b
    where
        lastline_a = last (image ta)
        firstline_b = head (image tb)


--  How to do the tile assembly
--   See each tile connection as a hash or int value (bit pattern)
--   Jump along hash collisions?
--  Flip & Turn -> 8 Configurations
--  Howto?
--   a) Start arbitrary card/config
--   b) Find top left from here or assemble everything in direction right/bottom from here

data PuzzleTile = PuzzleTile {
    pnum :: Int,
    l :: String,
    r :: String,
    t :: String,
    b :: String,
    ori :: Int}
    deriving (Eq, Show, Read)

tile2PuzzleTile tile = PuzzleTile pnum l r t b ori
    where
        t = head (image tile)
        b = last (image tile)
        l = head (transpose (image tile))
        r = last (transpose (image tile))
        ori = 0
        pnum = num tile
    