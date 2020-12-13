import Text.Printf

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


-- count s = show (length s) ++ "\n"

-- main    = interact (unlines . map show . lines)
-- main = interact print lines
-- main = print $ navturn 'E' 'R' 180
-- main = print $ movecomplete 0 0 'E' ["R180", "S2", "F10"]
-- main = print $ tail ["oans", "zwoa", "dra"]

main = do  
    inp <- getContents
    let finalpos = movecomplete 0 0 'E' (lines inp)
    
    putStr "finalpos: "
    print finalpos
    putStr "manhattan dist: "
    print $ manhattan finalpos

manhattan pos = (abs a) + (abs b)
    where
        (a, b) = pos

movecomplete x y dir cmds 
    | length remaining_cmds == 0  = (xnew, ynew)
    | otherwise                   = movecomplete xnew ynew dirnew remaining_cmds
    where 
        (xnew, ynew, dirnew, remaining_cmds) = moveonce x y dir cmds

navturn olddir dir degrees
    | degrees > 90 && dir == 'L' = navturn (turnleft olddir) dir (degrees - 90)
    | degrees > 90 && dir == 'R' = navturn (turnright olddir) dir (degrees - 90)
    | degrees == 90 && dir == 'L' = turnleft olddir
    | degrees == 90 && dir == 'R' = turnright olddir

moveonce :: Int -> Int -> Char -> [String] -> (Int, Int, Char, [String])
moveonce x y dir cmds = (xnew, ynew, dirnew, leftover_cmds)
    where
        nav = head cmds
        cmd = head nav
        amount = read (tail nav) :: Int

        dirnew 
            | cmd `elem` "LR" = navturn dir cmd amount
            | otherwise  = dir

        move 
            | cmd == 'F' = movevec dirnew
            | cmd `elem` "NEWS" = movevec cmd
            | otherwise = (0, 0)

        xnew = x + fst move * amount
        ynew = y + snd move * amount
        leftover_cmds = tail cmds
