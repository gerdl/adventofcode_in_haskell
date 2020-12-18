import System.IO( openFile, hClose, hGetContents )
import System.IO( IOMode( ReadMode ) )
import Data.List.Split (splitOn)
-- import Control.Monad


main = do  
    let list = []
    handle <- openFile "01/data_in.txt" ReadMode
    contents <- hGetContents handle
    
    print contents
    print $ do_parsing contents

    print $ find_combi (do_parsing contents)
    
    hClose handle


toint :: String -> Int
toint x = read x :: Int

do_parsing inp = map toint (splitOn "\n" inp)

find_combi nums = [(i,j,i*j) | i <- nums,
                               j <- nums,
                               i < j,
                               i + j == 2020]
