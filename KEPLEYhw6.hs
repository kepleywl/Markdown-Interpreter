{--
    William Kepley
--}

import  Data.Char

type Vars = String

data AExpr = Var Vars | Const Integer
            | Add AExpr AExpr | Sub AExpr AExpr
            | Mul AExpr AExpr | Div AExpr AExpr
    deriving Show

data BExpr = TT | FF
            | And BExpr BExpr | Or BExpr BExpr | Not BExpr
            | Eql AExpr AExpr | Lt AExpr AExpr 
    deriving Show

data Instr = Assign Vars AExpr
            | IfThenElse BExpr Instr Instr 
            | While BExpr Instr 
            | Do [Instr]
            | Nop
    deriving Show 

type Program = [Instr]

type Env = [(Vars, Integer)]

lookUp :: Vars -> Env -> Integer 
lookUp x ((y1, y2):ys) = if x == y1 then y2 else lookUp x ys 

update :: Vars -> AExpr -> Env -> Env 
update x (Const v) [] = [(x, v)]
update x (Const v) ((z1, z2):zs)  
    | x == z1 = (x, v) : zs 
    | otherwise = (z1, z2) : update x (Const v) zs 
update x v ys = update x (Const (evala ys v)) ys 

evala :: Env -> AExpr -> Integer 
evala e (Var x) = lookUp x e 
evala e (Const x) = x
evala e (Add x y) = evala e x + evala e y 
evala e (Sub x y) = evala e x - evala e y 
evala e (Mul x y) = evala e x * evala e y
evala e (Div x y) = evala e x `div` evala e y 

evalb :: Env -> BExpr -> Bool
evalb e TT = True
evalb e FF = False
evalb e (And x y) = evalb e x && evalb e y 
evalb e (Or x y) = evalb e x || evalb e y 
evalb e (Not x) = not $ evalb e x
evalb e (Eql x y) = evala e x == evala e y 
evalb e (Lt x y) = evala e x < evala e y 

exec :: Instr -> Env -> Env 
exec (Assign x y) e = update x y e 
exec (IfThenElse x y z) e 
    | evalb e x = exec y e
    | otherwise = exec z e 
exec (While x y) e 
    | evalb e x = exec (While x y) $ exec y e 
    | otherwise = e 
exec (Do []) e = e 
exec (Do (x:xs)) e = exec (Do xs) (exec x e)
exec Nop e = e 

data UOps = NotOp deriving Show 
data BOps = AddOp | SubOp | MulOp | DivOp | AndOp | OrOp | EqlOp | LtOp | AssignOp
    deriving Show 

data Token = VSym String | CSym Integer | BSym Bool 
    | UOp UOps | BOp BOps 
    | LPar | RPar | LBra | RBra | Semi
    | Keyword String
    | Err 
    | PA AExpr | PB BExpr | PI Instr 
    deriving Show

isVSym :: String -> Bool
isVSym "" = False 
isVSym (x:xs) = isLower x && q1 xs 
    where   q1 "" = True 
            q1 (y:ys) = (isAlpha y || isDigit y || y `elem` "-_'") && q1 ys 

isCSym :: String -> Bool
isCSym "" = False 
isCSym (x:xs) = isDigit x && q1 xs 
    where   q1 "" = True 
            q1 (y:ys) = (isDigit y && q1 ys) || (y == '.' && not (null ys) && q2 ys)
            q2 ys = all isDigit ys 

isBSym :: String -> Bool
isBSym "T" = True 
isBSym "F" = True 
isBSym _ = False 

classifyBSym :: String -> Bool 
classifyBSym "T" = True 
classifyBSym "F" = False 

classify :: String -> Token 
classify "(" = LPar 
classify ")" = RPar 
classify "{" = LBra
classify "}" = RBra 
classify ";" = Semi 

classify "+" = BOp AddOp 
classify "*" = BOp MulOp 
classify "-" = BOp SubOp
classify "/" = BOp DivOp 
classify "/\\" = BOp AndOp 
classify "\\/" = BOp OrOp 
classify "==" = BOp EqlOp 
classify "<" = BOp LtOp 
classify ":=" = BOp AssignOp 
classify "!" = UOp NotOp 

classify "while" = Keyword "while"
classify "IfThenElse" = Keyword "IfThenElse"
classify "if" = Keyword "if"
classify "then" = Keyword "then"
classify "else" = Keyword "else"
classify "nop" = Keyword "nop"

classify s | isVSym s = VSym s 
classify s | isCSym s = CSym (read s) 
classify s | isBSym s = BSym (classifyBSym s) 

classify _ = Err 

preproc :: String -> String 
preproc [] = []
preproc ('/':'\\':xs) = " /\\ " ++ preproc xs 
preproc ('\\':'/':xs) = " \\/ " ++ preproc xs 
preproc ('(':xs) = " ( " ++ preproc xs 
preproc (')':xs) = " ) " ++ preproc xs 
preproc ('{':xs) = " { " ++ preproc xs
preproc ('}':xs) = " } " ++ preproc xs
preproc (';':xs) = " ; " ++ preproc xs
preproc ('+':xs) = " + " ++ preproc xs
preproc ('*':xs) = " * " ++ preproc xs
preproc ('%':xs) = " % " ++ preproc xs
preproc ('^':xs) = " ^ " ++ preproc xs
preproc ('/':xs) = " / " ++ preproc xs
preproc ('-':'>':xs) = " -> " ++ preproc xs
preproc ('<':'-':'>':xs) = " <-> " ++ preproc xs
preproc ('<':'=':xs) = " <= " ++ preproc xs
preproc ('<':xs) = " < " ++ preproc xs
preproc ('=':'=':xs) = " == " ++ preproc xs
preproc (':':'=':xs) = " := " ++ preproc xs
preproc (x:xs) = x : preproc xs

lexer :: String -> [Token] 
lexer s = map classify (words (preproc s)) 


parser :: [Token] -> Instr 
parser input = case sr [] (LBra : input ++ [RBra]) of
    [PI i] -> i 
    ps -> error ("No parser:" ++ show ps)

sr :: [Token] -> [Token] -> [Token]
sr (VSym v : stack)     input = sr (PA (Var v) : stack) input 
sr (CSym c : stack)     input = sr (PA (Const c) : stack) input 
sr (PA e2 : BOp AddOp : PA e1 : stack) input = sr (PA (Add e1 e2) : stack) input 
sr (PA e2 : BOp MulOp : PA e1 : stack) input = sr (PA (Mul e1 e2) : stack) input
sr (PA e2 : BOp DivOp : PA e1 : stack) input = sr (PA (Div e1 e2) : stack) input 
sr (RPar : PA e : LPar : stack)         input = sr (PA e : stack) input 
sr (RPar : PB e : LPar : stack)         input = sr (PB e : stack) input 
sr (RPar : PI i : LPar : stack)         input = sr (PI i : stack) input
sr (PA a2 : BOp EqlOp : PA a1 : stack) input = sr (PB (Eql a1 a2) : stack) input 
sr (PA a2 : BOp LtOp : PA a1 : stack) input = sr (PB (Lt a1 a2) : stack) input 
sr (PB b2 : BOp OrOp : PB b1 : stack) input = sr (PB (Or b1 b2) : stack) input 
sr (PB b2 : BOp AndOp : PB b1 : stack) input = sr (PB (And b1 b2) : stack) input 
sr (PB b : UOp NotOp : stack) input             = sr (PB (Not b) : stack) input 
sr (BSym False : stack) input                   = sr (PB FF : stack) input
sr (BSym True : stack)  input                   = sr (PB TT : stack) input
sr (PA a : BOp AssignOp : PA (Var c) : stack) input = sr (PI (Assign c a) : stack) input
sr (PI i : PB b : Keyword "while" : stack) input = sr (PI (While b i) : stack) input 
sr (PI i2 : PI i1 : PB b : Keyword "IfThenElse" : stack) input = sr (PI (IfThenElse b i1 i2) : stack) input
sr (PI i2 : Keyword "else" : PI i1 : Keyword "then" : PB b : Keyword "if" : stack) input = sr (PI (IfThenElse b i1 i2) : stack) input

sr (RBra : PI i : stack) input = sr (PI (Do [i]) : stack) input 
sr (RBra : stack) input = sr (PI (Do []) : stack) input 
sr (PI (Do s) : Semi : PI i : stack) input = sr (PI (Do (i:s)) : stack) input 
sr (PI (Do s) : LBra : stack) input = sr (PI (Do s) : stack) input 
sr stack    (i:input) = sr (i:stack) input 
sr stack [] = stack 

createProgram :: Instr -> Program 
createProgram (Do xs) = xs 

executeProgram :: Program -> Env 
executeProgram xs = exec (Do xs) []

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
    let exe = executeProgram program
    putStrLn (show exe)