import Data.List.Split (splitOn)
import Data.List (minimum)

busraw = "13,x,x,x,x,x,x,37,x,x,x,x,x,461,x,x,x,x,x,x,x,x,x,x,x,x,x,17,x,x,x,x,19,x,x,x,x,x,x,x,x,x,29,x,739,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,x,x,x,23"
--busraw = "13,x,x,x,x,x,x,37,x,x,x,x,x,461,x,x,x,x,x,x,x,x,x,x,x,x,x,17"
--busraw = "7,13,x,x,59,x,31,19"
--busraw = "1,3,5"
--busraw = "3,5,7"

buslines_str = splitOn "," busraw


data Bus = Bus { 
                bid :: Int, 
                offset :: Int
               } 
               deriving (Eq, Show, Read)  


toint :: String -> Int
toint x = read x :: Int

present_buslines = [Bus (toint bid) off | (bid,off) <- zip buslines_str [0..],
                                           bid /= "x"]

maxnum buslines = product [bid | (Bus bid off) <- buslines]

-- Naive search iteration... takes too long for full list!
naive_times = [(t, all_bus_corret present_buslines t) | 
             k <- [0..my_maxnum],
             let t = k,
             all_bus_corret present_buslines t == True || k `mod` 1000000000 == 0
        ]
        where
            my_maxnum = maxnum present_buslines

-- smarter, iterative approach:
solution_time (bushead:bustail)
    | bustail == []    = bid_next - off_next        -- empty tail: recursion start
    | otherwise        = t_next
    where 
        t_tail = solution_time bustail
        prod_tail = product [bid | (Bus bid _) <- bustail]
        bid_next = bid bushead
        off_next = offset bushead
        t_next = minimum [t | k <- [0..bid_next],
                            let t = k * prod_tail + t_tail,
                            (t + off_next) `mod` bid_next == 0
                ]


bus_correct_time (Bus bid off) t = (t + off) `mod` bid == 0

all_bus_corret blist t = False `notElem` [bus_correct_time bus t | bus <- blist]


main = do
    putStrLn $ "Present buslines: " ++ show present_buslines
    putStrLn $ "Maxnum " ++ show (maxnum present_buslines)
    -- putStrLn $ "Naive solution times: " ++ show naive_times

    let soltime = solution_time present_buslines
    putStrLn $ "sol time: " ++ show soltime
    putStrLn $ "sol time: " ++ show (all_bus_corret present_buslines soltime)


{-

busses 3,5,7

maxnum = 3*5*7

    3   5   7
0   x   x   x
1   
2
3   x
4
5       x
6   x   
7           x
8
9   x
10      x
11
12  x
13
14          x
15  x   x
16
...

-}