import Data.List.Split (splitOn)

-- busraw = "13,x,x,x,x,x,x,37,x,x,x,x,x,461,x,x,x,x,x,x,x,x,x,x,x,x,x,17,x,x,x,x,19,x,x,x,x,x,x,x,x,x,29,x,739,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,x,x,x,23"
busraw = "7,13,x,x,59,x,31,19"

buslines_str = splitOn "," busraw


data Bua = Bus { 
                bid :: Int, 
                offset :: Int
               } 
               deriving (Eq, Show, Read)  


toint :: String -> Int
toint x = read x :: Int

present_buslines = [Bus (toint bid) off | (bid,off) <- zip buslines_str [0..],
                                           bid /= "x"]

times = [t | t <- [0..],
             -- let t = k*59 - 4
             all_bus_corret present_buslines t == True
        ]

bus_correct_time (Bus bid off) t = (t + off) `mod` bid == 0

all_bus_corret blist t = False `notElem` [bus_correct_time bus t | bus <- blist]


-- main = print $ bus_correct_time (present_buslines !! 3) 123456
-- main = print $ all_bus_corret present_buslines 123456
-- main = print $ all_bus_corret present_buslines 1068781
main = do
    print "Hallo"
    -- print [1..]
    print $ times
