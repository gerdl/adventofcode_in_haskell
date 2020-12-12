--import Dict exposing (Dict)
import Data.Set

--users : Dict String User
turnleft_dict =
  fromList
    [ 
        ('N', 'W'), 
        ('W', 'S'), 
        ('S', 'E'),
        ('E', 'N')
    ]

turnright_dict =
  fromList
    [ 
        ('N', 'E'), 
        ('E', 'S'), 
        ('S', 'W'),
        ('W', 'N')
    ]

-- main    = interact (unlines . map show . lines)

-- count s = show (length s) ++ "\n"

main = print $ moveme 0 0 'E' ["N1", "S2"]
-- main = print $ tail ["oans", "zwoa", "dra"]

-- moveme :: Int Int [String] -> (Int, Int, [String])
moveme x y dir cmds = (xnew, ynew, dirnew, leftover_cmds)
    where
        nav = head cmds
        cmd = head nav
        dirnew = 'N'
        xnew = x + 1
        ynew = y + 2
        leftover_cmds = tail cmds
