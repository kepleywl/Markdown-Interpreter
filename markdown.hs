{-
    William Kepley
-}

data Exp =  Text String | H1 Exp | H2 Exp | H3 Exp | H4 Exp | H5 Exp 
            | H6 Exp | Par Exp | Bold Exp | Under Exp | OList [Exp] 
            | UList [Exp] | LI Exp | Emph Exp 


classify :: String -> Exp 
classify ('#':'#':'#':'#':'#':'#':s) = H6 (classify s)
classify ('#':'#':'#':'#':'#':s) = H5 (classify s)
classify ('#':'#':'#':'#':s) = H4 (classify s)
classify ('#':'#':'#':s) = H3 (classify s)
classify ('#':'#':s) = H2 (classify s)
classify ('#':s) = H1 (classify s)

sr :: [Exp] -> [Exp] -> [Exp]
-- sr (VSym v : stack)     input = sr (PA (Var v) : stack) input 
-- sr (CSym c : stack)     input = sr (PA (Const c) : stack) input 
-- sr (PA e2 : BOp AddOp : PA e1 : stack) input = sr (PA (Add e1 e2) : stack) input 
-- sr (PA e2 : BOp MulOp : PA e1 : stack) input = sr (PA (Mul e1 e2) : stack) input
-- sr (PA e2 : BOp DivOp : PA e1 : stack) input = sr (PA (Div e1 e2) : stack) input 
-- sr (RPar : PA e : LPar : stack)         input = sr (PA e : stack) input 
-- sr (RPar : PB e : LPar : stack)         input = sr (PB e : stack) input 
-- sr (RPar : PI i : LPar : stack)         input = sr (PI i : stack) input
-- sr (PA a2 : BOp EqlOp : PA a1 : stack) input = sr (PB (Eql a1 a2) : stack) input 
-- sr (PA a2 : BOp LtOp : PA a1 : stack) input = sr (PB (Lt a1 a2) : stack) input 
-- sr (PB b2 : BOp OrOp : PB b1 : stack) input = sr (PB (Or b1 b2) : stack) input 
-- sr (PB b2 : BOp AndOp : PB b1 : stack) input = sr (PB (And b1 b2) : stack) input 
-- sr (PB b : UOp NotOp : stack) input             = sr (PB (Not b) : stack) input 
-- sr (BSym False : stack) input                   = sr (PB FF : stack) input
-- sr (BSym True : stack)  input                   = sr (PB TT : stack) input
-- sr (PA a : BOp AssignOp : PA (Var c) : stack) input = sr (PI (Assign c a) : stack) input
-- sr (PI i : PB b : Keyword "while" : stack) input = sr (PI (While b i) : stack) input 
-- sr (PI i2 : PI i1 : PB b : Keyword "IfThenElse" : stack) input = sr (PI (IfThenElse b i1 i2) : stack) input
-- sr (PI i2 : Keyword "else" : PI i1 : Keyword "then" : PB b : Keyword "if" : stack) input = sr (PI (IfThenElse b i1 i2) : stack) input

-- sr (RBra : PI i : stack) input = sr (PI (Do [i]) : stack) input 
-- sr (RBra : stack) input = sr (PI (Do []) : stack) input 
-- sr (PI (Do s) : Semi : PI i : stack) input = sr (PI (Do (i:s)) : stack) input 
-- sr (PI (Do s) : LBra : stack) input = sr (PI (Do s) : stack) input 

sr (H1 s : stack) input = sr (H1 s : stack) input 
sr stack    (i:input) = sr (i:stack) input 
sr stack [] = stack 

lexer :: String -> [Exp] 
lexer s = map classify (words (s)) 

parser :: [Exp] -> String
parser input = case sr [] input of
    Exp -> i 
    ps -> error ("No parser:" ++ show ps)

main :: IO ()
main = do 
    putStrLn "Enter file name: "
    filename <- getLine 
    contents <- readFile filename 
    let analyzed = lexer contents 
    -- putStrLn "Here is the result of lexical analysis: "
    -- putStrLn (show analyzed)
    -- putStrLn "-------------------------------"
    let parsed = parser analyzed 
    -- putStrLn "Here is the result of parsing: "
    -- putStrLn (show parsed)
    let program = createProgram parsed 
    putStrLn (show program)