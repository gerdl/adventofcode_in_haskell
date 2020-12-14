import Data.List.Split (splitOn)

starttime = 1001287
busraw = "13,x,x,x,x,x,x,37,x,x,x,x,x,461,x,x,x,x,x,x,x,x,x,x,x,x,x,17,x,x,x,x,19,x,x,x,x,x,x,x,x,x,29,x,739,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,x,x,x,23"

buslines_str = splitOn "," busraw

toint :: String -> Int
toint x = read x :: Int

present_buslines_str = [v | v <- buslines_str,
                        v /= "x"]
buslines_int = map toint present_buslines_str

time_till_next_bus bus_id 
    | starttime `mod` bus_id == 0    = 0
    | otherwise                      = bus_id - starttime `mod` bus_id

main = print $ map time_till_next_bus buslines_int
-- main = print $ present_buslines_str
