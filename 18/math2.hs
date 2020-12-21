import System.IO( openFile, hClose, hGetContents )
import System.IO( IOMode( ReadMode ) )
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Char (isNumber, ord)
import Data.Maybe (isNothing, fromJust)

-- switch between trace logging and no verbose output
trace _ fn = fn 
-- import Debug.Trace (trace)

--example = parseStringExpression "2+(3+(4+5))*(6+2)+2"
--example = parseStringExpression "2+3+(4+5+6+2+2)"
--example = parseStringExpression "1+(2+3)"
--example = parseStringExpression "((8+3+4+7*5*6)+(3+2)*(8+6*9*4+9+2))+3+8*(8+(4+9*2+8+6+9)*9*3*3+3)+9"
--example = parseStringExpression "((2+4*9)*(6+9*8+6)+6)+2+4*2"
--res = simplify_list_rec example
--main = print $ show example ++ " == " ++ show res


simplification_order = [EMOp '+', EMOp '*']

-- EMExpr -> EMLit
-- EMLit  -> EMLit
-- EMOp   -> EMOp
simplify_em_rec :: ExprMem -> ExprMem
simplify_em_rec (EMExpr exprlist) = res
    where 
        res = simplify_list_rec exprlist
simplify_em_rec em = em

-- [ExprMem] --> EMLit
-- simplify all member-EMExprs in emlist
-- then keep simplifying this expression in simplification_order until we have a literal!
simplify_list_rec :: [ExprMem] -> ExprMem
simplify_list_rec emlist = result_literal
    where
        -- first transform every expression to literal or operator:
        simple_expression = [simplify_em_rec em | em <- emlist]

        -- find the first matching operator and its index
        (op,op_index) = try_operators_rec simplification_order simple_expression

        -- Now we simplify one step and call ourselves recursively:
        -- do one simplification step:
        --  0  1  2  3  4  5  6      length = 7
        -- (a, +, c, *, d, -, f)
        --           ^------ op_index = 3
        --  
        -- (a, +, ctd,     -, f)
        --  0  1  2        3  4
        new_literal = use_op (simple_expression!!(op_index-1)) (simple_expression!!(op_index)) (simple_expression!!(op_index+1))
        simpler = [em | k <- [0 .. (length simple_expression - 3)],
                                    let em 
                                            | k < (op_index-1)    = simple_expression !! k
                                            | k == (op_index-1)   = new_literal
                                            | k > (op_index-1)    = simple_expression !! (k+2) ]

        result_literal 
            | length simpler == 1    = simpler !! 0
            | otherwise              = trace ("recursing with " ++ show simpler)
                                       $ simplify_list_rec simpler

try_operators_rec myop_order exprList
    | isNothing myop_index    = try_operators_rec (tail myop_order) exprList
    | otherwise               = (myop, fromJust myop_index)
    where 
        myop = head myop_order
        myop_index = myop `elemIndex` exprList


use_op :: ExprMem -> ExprMem -> ExprMem -> ExprMem
use_op (EMLit l) (EMOp '+') (EMLit r) = EMLit (l+r)
use_op (EMLit l) (EMOp '*') (EMLit r) = EMLit (l*r)
use_op l op r = error ("Can't use op: " ++ show l ++ show op ++ show r)

fromEMLit (EMLit l) = l

data ExprMem = EMOp Char | EMLit Int | EMExpr [ExprMem]
            deriving (Eq, Read)  
instance Show ExprMem where
    show (EMOp op) = show op
    show (EMLit lit) = show lit
    show (EMExpr ex) = show ex



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
        firstR = head rs
        lev' 
            | firstR == '('   = lev + 1
            | firstR == ')'   = lev - 1
            | otherwise       = lev
        (ls', rs', rec_lev) = extract_parenthesis_fromleft_rec (ls ++ [firstR]) (tail rs) lev'

extract_parenthesis_fromleft inp = (ls,rs)
    where
        ls = tail (init lsraw)
        (lsraw,rs,_) = extract_parenthesis_fromleft_rec "(" (tail inp) 1



main = do  
    let list = []
    handle <- openFile "18/data_in.txt" ReadMode
    contents <- hGetContents handle
    
    let inp = do_parsing contents
    let res_list = map (fromEMLit . simplify_list_rec . parseStringExpression) inp

    print $ res_list
    print $ ("Sum of results: " ++ show (sum res_list))

    hClose handle


tochars inp = [c | c <- inp,
                   c /= ' ']
do_parsing inp = map tochars (splitOn "\n" inp)
c2int c
    | c `elem` ['0'..'9']     = ord c - ord '0'
    | otherwise               = error ("Bad char for conversion to int: " ++ show c)

