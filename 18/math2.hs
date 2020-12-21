import System.IO( openFile, hClose, hGetContents )
import System.IO( IOMode( ReadMode ) )
import Data.List.Split (splitOn)
import Data.Char (isNumber, ord)

-- switch between trace logging and no verbose output
--trace _ fn = fn 
import Debug.Trace (trace)

{-
  new approach:

    ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2
     ----------- - --------------- - -    -   -   -
    -----------------------------------  
    -----------------------------------------------

    Expression = List(Expression)|Operator|Literal

-}


-- main = print $ parseStringExpression "(3+(4+5))+(6*2)"
-- main = print $ parseStringExpression "3+4+5+6+2"


-- main = print $ use_op (EMLit 5) (EMOp '+') (EMExpr (parseStringExpression "3+4+5+6+2"))
-- 
-- main = print $ simplify_expr_4_op (EMOp '+') (EMExpr [EMLit 5])
-- main = print $ simplify_expr_4_op (EMOp '+') (EMLit 5)
-- main = print $ simplify_expr_4_op (EMOp '+') (EMExpr [EMLit 5, EMOp '*', EMLit 7, EMOp '+', EMLit 9])
-- main = print $ simplify_expr_4_op (EMOp '*') (EMExpr [EMLit 5, EMOp '*', EMLit 7, EMOp '+', EMLit 9])
-- example = parseStringExpression "2+(3+(4+5))+(6+2)+2"
example = parseStringExpression "2+3+(4+5+6+2+2)"
main = print $ simplify_expr_4_op (EMOp '+') (EMExpr example)
--main = print $ example

--            A,+,B,*,C,+,D
--            l o r rest---
-- try with:  -----
-- recurse A:  rec(lor ++ rest)             | op_applicable
-- recurse B:  l ++ rec(r ++ rest)         | ! op_applicable


simplify_expr_4_op :: ExprMem -> ExprMem -> ExprMem
simplify_expr_4_op _ (EMLit v) = EMLit v
simplify_expr_4_op op (EMExpr [EMLit lit]) = EMLit lit
--simplify_expr_4_op op (EMExpr [em]) = simplify_expr_4_op op em    -- not sure if this is a good idea...
simplify_expr_4_op op (EMExpr expr) = trace ("remaining=" ++ show remaining ++ " from " ++ show expr ++ " op_result=" ++ show op_result ++ " applicable=" ++ show dbg_applicable) 
                                      $ remaining
    where
        l       = head expr
        myop    = head (tail expr)
        r       = head (tail (tail expr))
        rest    = tail (tail (tail expr))
        l_ex    = simplify_expr_4_op op l
        r_ex    = simplify_expr_4_op op r

        op_applicable (EMLit _) (EMLit _)    = op == myop
        op_applicable _ _                    = False
        op_result = use_op l_ex op r_ex
        
        dbg_applicable = op_applicable l r

        -- recurse
        remaining 
            | op_applicable l r   = simplify_expr_4_op op $ EMExpr (op_result ++ rest)
            | otherwise           = join_EMExpr  (EMExpr ([l] ++ [myop]))  $ simplify_expr_4_op op (EMExpr (tail (tail expr))) 

use_op :: ExprMem -> ExprMem -> ExprMem -> [ExprMem]
use_op (EMLit l) (EMOp '+') (EMLit r) = [EMLit (l+r)]
use_op (EMLit l) (EMOp '*') (EMLit r) = [EMLit (l*r)]
-- use_op l op r = [l, op, r]

join_EMExpr (EMExpr ea) (EMExpr eb) = EMExpr (ea ++ eb)
join_EMExpr (EMExpr ea) (EMLit lit) = EMExpr (ea ++ [EMLit lit])
join_EMExpr a b = trace ("Uncommon join: " ++ show a ++ " || " ++ show b) $ EMExpr [EMLit 999]

op_applicable (EMLit _) (EMLit _) = True
op_applicable _ _ = False


data ExprMem = EMOp Char | EMLit Int | EMExpr [ExprMem]
            deriving (Eq, Show, Read)  
--instance Show ExprMem where
--    show (EMOp op) = show op
--    show (EMLit lit) = show lit
--    show (EMExpr ex) = show ex



-- Try parsing without explicit types -> Expression = List, Op = Char, Number = Int
parseStringExpression []  = []
parseStringExpression inp
    | length inp == 1     = [EMLit (c2int c)]
    | isNumber c          = parseStringExpression [c] ++ (parseStringExpression rest)
    | c `elem` ['+','*']  = [EMOp c] ++ (parseStringExpression rest)
    | c == '('            = [EMExpr (parseStringExpression lpar)] ++ (parseStringExpression rpar)
    | otherwise           = error ("Unable to parse." ++ show c ++ show rest)
    where
        c = head inp
        rest = tail inp
        (lpar, rpar) = extract_parenthesis_fromleft inp

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


{-
main = do  
    let list = []
    handle <- openFile "18/data_in.txt" ReadMode
    contents <- hGetContents handle
    
    --print $ 

    --print $ do_parsing contents

    let inp = do_parsing contents
    let res_list = map eval_strange_math inp

    --print $ res_list

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
-}

tochars inp = [c | c <- inp,
                   c /= ' ']
do_parsing inp = map tochars (splitOn "\n" inp)
c2int c
    | c `elem` ['0'..'9']     = ord c - ord '0'
    | otherwise               = error ("Bad char for conversion to int: " ++ show c)

