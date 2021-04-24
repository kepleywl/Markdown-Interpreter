{-
    William Kepley
-}

-- data Tag =  H1 [Exp] | H2 [Exp] | H3 [Exp] | H4 [Exp] | H5 [Exp] 
--             | H6 [Exp] | Par [Exp] | Bold [Exp] | Under [Exp] | OList [Exp] 
--             | UList [Exp] | LI [Exp] | Emph [Exp] | P [Exp]
--             deriving Show

data Exp =  Text String | H1 [Exp] | H2 [Exp] | H3 [Exp] | H4 [Exp] | H5 [Exp] 
            | H6 [Exp] | Par [Exp] | Bold [Exp] | Under [Exp] | OList [Exp] 
            | UList [Exp] | LI [Exp] | Emph [Exp] | Hash | Word String 
            | HTML [Exp] | LHTML | RHTML | NewLine | P [Exp] | DA 
            deriving Show


type HTML = [Exp] 

preproc :: String -> String 
preproc [] = [] 
preproc ('*' : '*' : xs) = " ** " ++ preproc xs
preproc ('#' : xs) = " # " ++ preproc xs 
preproc (' ' : ' ' : '\n' : xs) = " <br> " ++ preproc xs 
preproc ('\n' : xs) = " \\n " ++ preproc xs
preproc (x : xs) = x : preproc xs


classify :: String -> Exp 
-- classify ('#':'#':'#':'#':'#':'#':s) = H6 (classify s)
-- classify ('#':'#':'#':'#':'#':s) = H5 (classify s)
-- classify ('#':'#':'#':'#':s) = H4 (classify s)
-- classify ('#':'#':'#':s) = H3 (classify s)
-- classify ('#':'#':s) = H2 (classify s)
-- classify ('#':s) = H1 (classify s)
classify "#" = Hash
classify "**" = DA
classify "\\n" = NewLine 

classify s = Word s 


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

sr (NewLine : Word s : stack) input = sr (Text s : stack) input
sr (Word t : Word s : stack) input = sr (Text (s ++ " " ++ t) : stack) input
sr (Word w : Text t : stack) input = sr (Text (t ++ " " ++ w) : stack) input 
sr (Bold x : Text t : stack) input = sr (Comb [Text t, Bold x] : stack) input

sr (DA : Text t : DA : stack) input = sr (Bold [Text t] : stack) input 
sr (DA : Word w : DA : stack) input = sr (Bold [Word w] : stack) input 

sr (Text s : Hash : stack) input = sr (H1 [Text s] : stack) input 
sr (Word w : H1 [Text t] : stack) input = sr (H1 [Text (t ++ " " ++ w)] : stack) input
sr (Bold b : H1 h : stack) input = sr (H1 (h ++ [Bold b]) : stack) input
sr (NewLine : H1 x : stack) input = sr (H1 x : stack) input


sr (RHTML : stack) input = sr (HTML [] : stack) input
sr (LHTML : stack) input = sr (stack) input
sr (HTML xs : H1 x : stack) input = sr (HTML ((H1 x):xs) : stack) input 

sr stack    (i:input) = sr (i:stack) input 
sr stack [] = stack 

lexer :: String -> [Exp] 
lexer s = map classify (words (preproc s)) 

parser :: [Exp] -> HTML  
parser input = case sr [] (LHTML : input ++ [RHTML]) of
    [HTML e] -> e
    ps -> error ("No parser:" ++ show ps)

convert ::  HTML -> String 
convert [] = ""
convert (Word w : xs) = w ++ convert xs
convert (Text t : xs) = t ++ convert xs
convert (Bold b : xs) = "<strong>" ++ convert b ++ "</strong>" ++ convert xs
convert (H1 x : xs) = "<h1>" ++ convert x ++ "</h1>" ++ convert xs

main :: IO ()
main = do 
    putStrLn "Enter file name without extension: "
    filename <- getLine 
    contents <- readFile (filename ++ ".md")
    let analyzed = lexer contents 
    -- putStrLn "Here is the result of lexical analysis: "
    -- putStrLn (show analyzed)
    -- putStrLn "-------------------------------"
    let parsed = parser analyzed 
    -- putStrLn "Here is the result of parsing: "
    -- putStrLn (show parsed)
    -- let program = createProgram parsed
    let html = convert parsed 
    writeFile (filename ++ ".html") html
    putStrLn (show parsed)