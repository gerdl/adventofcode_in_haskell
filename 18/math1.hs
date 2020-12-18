import System.IO( openFile, hClose, hGetContents )
import System.IO( IOMode( ReadMode ) )
import Data.List.Split (splitOn)
import Data.Char (isNumber, ord)

-- switch between trace logging and no verbose output
trace _ fn = fn 
--import Debug.Trace (trace)



main = do  
    let list = []
    handle <- openFile "18/data_in.txt" ReadMode
    contents <- hGetContents handle
    
    print $ do_parsing contents

    let inp = do_parsing contents
    let res_list = map eval_strange_math inp

    print $ res_list

    print $ ("Hello " ++ ['!'])   -- join character from right does not work with :
    print $ ("Sum of results: " ++ show (sum res_list))

    --print $ extract_parenthesis_fromleft_rec "(" "ga(t)ta(ca))foo(b00)ook" 1
    --print $ extract_parenthesis_fromleft "(ga(t)ta(ca))foo(b00)ook"
    --print $ strange_reverse "(foo)wgo(bar)"
    --print $ extract_parenthesis_fromright "(foo)wgo(bar)"
    --print $ extract_parenthesis_fromright "1+2*3+4*(5+6)"
    
    {-
    print $ extract_parenthesis_fromright "((2+4*9)*(6+9*8+6)+6)"
    print $ eval_strange_math "1+2*(3+4)*5+6"
    print $ eval_strange_math "1+(2*3)+(4*(5+6))"
    print $ eval_strange_math "5*9*(7*3*3+9*3+(8+6*4))"
    print $ eval_strange_math "((2+4*9)*(6+9*8+6)+6)+2+4*2"
    -}

    hClose handle


tochars inp = [c | c <- inp,
                   c /= ' ']
do_parsing inp = map tochars (splitOn "\n" inp)


c2int c
    | c `elem` ['0'..'9']     = ord c - ord '0'
    | otherwise               = error ("Bad char for conversion to int: " ++ show c)

c2op c opA opB = case c of
    '+' -> opA + opB
    '*' -> opA * opB
    otherwise -> error ("Strange operator character: " ++ [c])

-- This evaluation actually means that we're evaluating the expressions right to left, e.g.
--   (--start--) + 7  --> then recurse on (--start--)
--                 ^- c
--               ^--- op
eval_strange_math inp
    | length inp == 1   = c2int (head inp)
    | isNumber c        = trace ("esm " ++ show start ++ " " ++ show op ++ " " ++ show c) 
                          $ myOp (c2int c) (eval_strange_math start)
    | c == ')'          = trace ("esm-br ")
                          $ eval_parenthesis_expr inp
    | otherwise         = error ("Unable to parse." ++ show c ++ show op ++ show start)
    where
        myOp = c2op op
        start = init (init inp)
        op = last (init inp)
        c = last inp

{-
   In case of parentheses:
    (ook)+(f(oo)p)
          --------  ~ rs
    ------          ~ ls     --> op = last ls
-}
eval_parenthesis_expr inp = trace ("parenthesis-expr: " ++ inp ++ " op=" ++ [op] ++ " ls=" ++ ls ++ " rs=" ++ rs)
        $ res
    where
        (ls, rs) = extract_parenthesis_fromright inp
        myOp = c2op op
        op = last ls
        res
            | length ls > 0      = myOp (eval_strange_math (init ls)) (eval_strange_math rs)
            | otherwise          = eval_strange_math rs

extract_parenthesis_fromright inp = (strange_reverse rs, strange_reverse ls)
    where
        (ls, rs) = extract_parenthesis_fromleft (strange_reverse inp)

strange_reverse x = reverse (map f x)
    where
        f c
            | c == '('    = ')'
            | c == ')'    = '('
            | otherwise   = c

{-
"","(CACA(GATT)ACA)GATTACA",0   ==>   "(CACA(GATT)ACA)","GATTACA",0

"",                    "(CACA(GATT)ACA)GATTACA"   ,0 
"(",                    "CACA(GATT)ACA)GATTACA"   ,1
"(C",                    "ACA(GATT)ACA)GATTACA"   ,1
            ...
"(CACA(GATT)ACA)",                    "GATTACA"   ,0 
-}
extract_parenthesis_fromleft_rec ls rs 0 =  (ls, rs, 0)
extract_parenthesis_fromleft_rec ls rs lev =  (ls', rs', lev')
    where
        firstR = trace ("rs=" ++ show rs)
                 $ head rs
        lev' 
            | firstR == '('   = lev + 1
            | firstR == ')'   = lev - 1
            | otherwise       = lev
        (ls', rs', rec_lev) = extract_parenthesis_fromleft_rec (ls ++ [firstR]) (tail rs) lev'

extract_parenthesis_fromleft inp = (ls,rs)
    where
        ls = tail (init lsraw)
        (lsraw,rs,_) = extract_parenthesis_fromleft_rec "(" (tail inp) 1