--import Dict exposing (Dict)
import Data.Map as Map

--users : Dict String User
turnleft :: Char -> Char
turnleft d =
    case d of
        'N' -> 'W'
        'W' -> 'S'
        'S' -> 'E'
        'E' -> 'N'

turnright d =
    case d of
        'N' -> 'E'
        'E' -> 'S'
        'S' -> 'W'
        'W' -> 'N'

movevec d =
    case d of
        'N' -> (0, 1)
        'E' -> (1, 0)
        'S' -> (0, -1)
        'W' -> (-1, 0)

-- main    = interact (unlines . map show . lines)

-- count s = show (length s) ++ "\n"

-- main = print $ navturn 'E' 'R' 180
main = print $ movecomplete 0 0 'E' ["R180", "S2", "F10"]
-- main = print $ tail ["oans", "zwoa", "dra"]

movecomplete x y dir [] = (x, y)
movecomplete x y dir cmds 
    | length remaining_cmds == 0  = (xnew, ynew)
    | otherwise                   = movecomplete xnew ynew dirnew remaining_cmds
    where 
        (xnew, ynew, dirnew, remaining_cmds) = moveme x y dir cmds

navturn olddir dir degrees
    | degrees > 90 && dir == 'L' = navturn (turnleft olddir) dir (degrees - 90)
    | degrees > 90 && dir == 'R' = navturn (turnright olddir) dir (degrees - 90)
    | degrees == 90 && dir == 'L' = turnleft olddir
    | degrees == 90 && dir == 'R' = turnright olddir

moveme :: Int -> Int -> Char -> [String] -> (Int, Int, Char, [String])
moveme x y dir cmds = (xnew, ynew, dirnew, leftover_cmds)
    where
        nav = head cmds
        cmd = head nav
        amount = read (tail nav) :: Int

        dirnew 
            | cmd == 'L' = navturn dir cmd amount
            | cmd == 'R' = navturn dir cmd amount
            | otherwise  = dir

        move 
            | cmd == 'F' = movevec dirnew
            | cmd `elem` "NEWS" = movevec cmd
            | otherwise = (0, 0)

        xnew = x + fst move * amount
        ynew = y + snd move * amount
        leftover_cmds = tail cmds
