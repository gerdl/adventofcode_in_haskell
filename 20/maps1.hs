import System.IO( openFile, hClose, hGetContents )
import System.IO( IOMode( ReadMode ) )
import Data.List.Split (splitOn, splitOneOf)
import Data.Char (isNumber, ord)

-- switch between trace logging and no verbose output
--trace _ fn = fn 
import Debug.Trace (trace)


main = do  
    handle <- openFile "20/demo_1.in.txt" ReadMode
    contents <- hGetContents handle
    
    let tileset = readMultipleTiles contents

    print $ tileset

    printTile (tileset !! 1)

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
