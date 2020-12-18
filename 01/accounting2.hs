import System.IO( openFile, hClose, hGetContents )
import System.IO( IOMode( ReadMode ) )
import Data.List.Split (splitOn)
import Data.List (sort)

-- switch between trace logging and no verbose output
trace _ fn = fn 
-- import Debug.Trace (trace)

main = do  
    handle <- openFile "01/data_in.txt" ReadMode
    contents <- hGetContents handle
    
    let sorted = sort (do_parsing contents)

    -- in case you want to take a look at some numbers:
    --print contents
    --print $ sorted

    print $ "naive   3-sum: " ++ show (find_combi3 (do_parsing contents))
    print $ "n log n 2-sum: " ++ show (find_sum2_rec sorted 2020)
    print $ "n**2    3-sum: " ++ show (find_sum3_rec sorted 2020)

    hClose handle

-- For reading/parsing inputs
toint :: String -> Int
toint x = read x :: Int
do_parsing inp = map toint (splitOn "\n" inp)

-- Naive solution for 3 numbers:
find_combi3 nums = [(i,j,k,i*j*k) | i <- nums,
                                    j <- nums,
                                    k <- nums,
                                    i < j,
                                    j < k,
                                    i + j + k == 2020]

-- n**2 (?) solution to three numbers on sorted lists
find_sum3_rec (numhead:numtail) target
    | length numtail >= 2     = trace ("fs3r: remains=" ++ show remains ++ "; head= " ++ show numhead) 
                                $ sols_here ++ sols_rec
    | otherwise               = []
    where
        -- remains: solution for the 2-sum values for the remaining 3-sum once numhead is subtracted
        remains = find_sum2_rec numtail (target - numhead)
        sols_rec  = find_sum3_rec numtail target
        sols_here = [(numhead,j,k, numhead*j*k) | (j,k) <- remains,
                                                  numhead + j + k == target]

-- n log n solution for two numbers on sorted list:
find_sum2_rec nums target
    | length nums == 2       = is_solution
    | fi + la > target       = find_sum2_rec (init nums) target
    | fi + la < target       = find_sum2_rec (tail nums) target
    | fi + la == target      = (fi, la) : (find_sum2_rec (tail nums) target)
    where
        fi = head nums
        la = last nums
        is_solution
            | fi + la /= target     = []
            | otherwise             = [(fi, la)]
