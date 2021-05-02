{-
    William Kepley
-}

data Tag =  H1 [Exp] | H2 [Exp] | H3 [Exp] | H4 [Exp] | H5 [Exp] 
            | H6 [Exp] | Bold [Exp] | OList [Exp] | Block [Exp]
            | UList [Exp] | LI [Exp] | P [Exp] | BR | EM [Exp]
            deriving Show

data Exp =  Text String | Hash | Word String | PA Tag | Comb [Exp]
            | HTML [Exp] | LHTML | RHTML | NewLine | DA | AA | TA 
            | BB | Tab
            deriving Show


type HTML = [Exp] 

preproc :: String -> String 
preproc [] = [] 
preproc ('*' : '*' : '*' : xs) = " *** " ++ preproc xs
preproc ('_' : '_' : '_' : xs) = " ___ " ++ preproc xs
preproc ('*' : '*' : xs) = " ** " ++ preproc xs
preproc ('_' : '_' : xs) = " __ " ++ preproc xs
preproc ('*' : xs) = " * " ++ preproc xs
preproc ('_' : xs) = " _ " ++ preproc xs
preproc ('#' : xs) = " # " ++ preproc xs 
preproc ('>' : xs) = " > " ++ preproc xs
preproc ('-' : xs) = " - " ++ preproc xs
preproc ('+' : xs) = " + " ++ preproc xs
preproc (' ' : ' ' : '\n' : xs) = " <br> " ++ preproc xs 
preproc ('\n' : xs) = " \\n " ++ preproc xs
preproc ('\t' : xs) = " \\t " ++ preproc xs
preproc (' ' : ' ' : ' ' : ' ' : xs) = " \\t " ++ preproc xs
preproc (x : xs) = x : preproc xs


classify :: String -> Exp 
classify "#" = Hash
classify "***" = TA 
classify "___" = TA
classify "**" = DA
classify "__" = DA
classify "*" = AA
classify "-" = AA
classify "+" = AA
classify "_" = AA 
classify "\\n" = NewLine 
classify "\\t" = Tab
classify "<br>" = PA (BR)
classify ">" = BB

classify s = Word s 


sr :: [Exp] -> [Exp] -> [Exp]
--Handling text
sr (NewLine : Word s : stack) input = sr (Text s : stack) input
sr (Word t : Word s : stack) input = sr (Text (s ++ " " ++ t) : stack) input
sr (Word w : Text t : stack) input = sr (Text (t ++ " " ++ w) : stack) input 
sr (PA (Bold x) : Text t : stack) input = sr (Comb [Text t, PA (Bold x)] : stack) input

--Bold and Emphasis 
sr (TA : Text t : TA : stack) input = sr (PA (Bold [PA (EM [Text t])]) : stack) input
sr (TA : Word w : TA : stack) input = sr (PA (Bold [PA (EM [Word w])]) : stack) input 


--Handling Bold Tag
sr (DA : Text t : DA : stack) input = sr (PA (Bold [Text t]) : stack) input 
sr (DA : Word w : DA : stack) input = sr (PA (Bold [Word w]) : stack) input 

--Emphasis Tag
sr (AA : Text t : AA : stack) input = sr (PA (EM [Text t]) : stack) input
sr (AA : Word w : AA : stack) input = sr (PA (EM [Word w]) : stack) input

--Handling H6 Tag
sr (Text s : Hash : Hash : Hash : Hash : Hash : Hash : stack) input = sr (PA (H6 [Text s]) : stack) input
sr (Word w : PA (H6 [Text t]) : stack) input = sr (PA (H6 [Text (t ++ " " ++ w)]) : stack) input
sr (PA t : PA (H6 h) : stack) input = sr (PA (H6 (h ++ [PA t])) : stack) input

--Handling H5 Tag
sr (Text s : Hash : Hash : Hash : Hash : Hash : stack) input = sr (PA (H5 [Text s]) : stack) input
sr (Word w : PA (H5 [Text t]) : stack) input = sr (PA (H5 [Text (t ++ " " ++ w)]) : stack) input
sr (PA t : PA (H5 h) : stack) input = sr (PA (H5 (h ++ [PA t])) : stack) input

--Handling H4 Tag
sr (Text s : Hash : Hash : Hash : Hash : stack) input = sr (PA (H4 [Text s]) : stack) input
sr (Word w : PA (H4 [Text t]) : stack) input = sr (PA (H4 [Text (t ++ " " ++ w)]) : stack) input
sr (PA t : PA (H4 h) : stack) input = sr (PA (H4 (h ++ [PA t])) : stack) input

--Handling H3 Tag
sr (Text s : Hash : Hash : Hash : stack) input = sr (PA (H3 [Text s]) : stack) input
sr (Word w : PA (H3 [Text t]) : stack) input = sr (PA (H3 [Text (t ++ " " ++ w)]) : stack) input
sr (PA t : PA (H3 h) : stack) input = sr (PA (H3 (h ++ [PA t])) : stack) input

--Handling H2 Tag
sr (Text s : Hash : Hash : stack) input = sr (PA (H2 [Text s]) : stack) input
sr (Word w : PA (H2 [Text t]) : stack) input = sr (PA (H2 [Text (t ++ " " ++ w)]) : stack) input
sr (PA t : PA (H2 h) : stack) input = sr (PA (H2 (h ++ [PA t])) : stack) input

--Handling H1 Tag
sr (Text s : Hash : stack) input = sr (PA (H1 [Text s]) : stack) input 
sr (Word w : PA (H1 [Text t]) : stack) input = sr (PA (H1 [Text (t ++ " " ++ w)]) : stack) input
sr (PA t : PA (H1 h) : stack) input = sr (PA (H1 (h ++ [PA t])) : stack) input

--Handling Blockquotes
sr (Text t : BB : stack) input = sr (PA (Block [Text t]) : stack) input
--sr (Text t : PA (Block x) : stack) input = sr (PA (Block (x ++ [Text t])) : stack) input
sr (Word w : PA (Block [Text t]) : stack) input = sr (PA (Block [Text (t ++ " " ++ w)]) : stack) input
sr (Word w : PA (Block [Word t]) : stack) input = sr (PA (Block [Text (t ++ " " ++ w)]) : stack) input
sr (PA (Block [Text t2]) : NewLine : PA (Block [Text t1]) : stack) input = sr (PA (Block [Text (t1 ++ " " ++ t2)]) : stack) input

--Nested Blockquote
sr (PA (Block b2) : BB : NewLine : PA (Block b1) : stack) input = sr (PA (Block (b1 ++ [PA (Block b2)])) : stack) input
sr (Text t2 : PA (Block [x, PA (Block [Text t1])]) : stack) input = sr (PA (Block [x, PA (Block [Text (t1 ++ " " ++ t2)])]) : stack) input
sr (Word w : PA (Block [x, PA (Block [Text t])]) : stack) input = sr (PA (Block [x, PA (Block [Text (t ++ " " ++ w)])]) : stack) input

--Ordered Lists
sr (Word "1." : stack) input = sr (PA (OList [PA (LI [Word ""])]) : stack) input 
sr (Word w2 : PA (OList [PA (LI [Word w1])]) : stack) input = sr (PA (OList [PA (LI [Text (w1 ++ w2)])]) : stack) input
sr (Word w : PA (OList [PA (LI [Text t])]) : stack) input = sr (PA (OList [PA (LI [Text (t ++ " " ++ w)])]) : stack) input
--Creating List Items
sr (NewLine : Text t : NewLine : PA (OList xs) : stack) input = sr (NewLine : PA (OList (xs ++ [PA (LI [Text (dfw t)])])) : stack) input
sr (NewLine : PA t : NewLine : PA (OList xs) :  stack) input = sr (NewLine : PA (OList (xs ++ [PA t])) : stack) input
--Sub lists depth 2
sr (NewLine : Text t : Tab : Tab : NewLine : PA (OList xs) : stack) input = sr (NewLine : PA (OList (xs ++ [PA (LI [Text (dfw t)])])) : stack) input
sr (Word w : Tab : NewLine : PA (OList xs2) : Tab : Tab : NewLine : PA (OList xs1) : stack) input = sr (Word w : Tab : NewLine : PA (OList (xs1 ++ [PA (OList xs2)])) : stack) input
sr (Word w : NewLine : PA (OList xs2) : Tab : Tab : NewLine : PA (OList xs1) : stack) input = sr (Word w : NewLine : PA (OList (xs1 ++ [PA (OList xs2)])) : stack) input
--Sub lists
sr (NewLine : Text t : Tab : NewLine : PA (OList xs) : stack) input = sr (NewLine : PA (OList (xs ++ [PA (LI [Text (dfw t)])])) : stack) input
sr (Word w : NewLine : PA (OList xs2) : Tab : NewLine : PA (OList xs1) : stack) input = sr (Word w : NewLine : PA (OList (xs1 ++ [PA (OList xs2)])) : stack) input
--Elements within ordered list
sr (NewLine : Text t : Tab : NewLine : NewLine : PA (OList xs) : stack) input = sr (PA (OList (xs ++ [PA (P [Text t])])) : stack) input 
sr (NewLine : PA p : Tab : NewLine : NewLine : PA (OList xs) : stack) input = sr (PA (OList (xs ++ [PA p])) : stack) input
--Unordered list within ordered list
sr (NewLine : Text t : AA : Tab : NewLine : PA (UList ys) : Tab : NewLine : PA (OList xs) : stack) input = sr (NewLine : PA (UList (ys ++ [PA (LI [Text t])])) : Tab : NewLine : PA (OList xs) : stack) input
sr (Word w: NewLine : PA (UList ys) : Tab : NewLine : PA (OList xs) : stack) input = sr (Word w :NewLine : PA (OList (xs ++ [PA (UList ys)])) : stack) input


--Unordered Sub lists depth 2
sr (NewLine : Text t : AA : Tab : Tab : NewLine : PA (UList ys) : Tab : Tab : NewLine : PA (UList xs) : stack) input = sr (NewLine : PA (UList (ys ++ [PA (LI [Text t])])) : Tab : Tab : NewLine : PA (UList xs) : stack) input
sr (AA : NewLine : PA (UList ys) : Tab : Tab : NewLine : PA (UList xs) : stack) input = sr (AA : NewLine : PA (UList (xs ++ [PA (UList ys)])) : stack) input
sr (AA : Tab : NewLine : PA (UList ys) : Tab : Tab : NewLine : PA (UList xs) : stack) input = sr (AA : Tab : NewLine : PA (UList (xs ++ [PA (UList ys)])) : stack) input
--Unordered Sub lists
sr (NewLine : Text t : AA : Tab : NewLine : PA (UList ys) : Tab : NewLine : PA (UList xs) : stack) input = sr (NewLine : PA (UList (ys ++ [PA (LI [Text t])])) : Tab : NewLine : PA (UList xs) : stack) input
sr (AA : NewLine : PA (UList ys) : Tab : NewLine : PA (UList xs) : stack) input = sr (AA : NewLine : PA (UList (xs ++ [PA (UList ys)])) : stack) input
--Creating Unordered Lists
sr (NewLine : Text t : AA : stack) input = sr (NewLine : PA (UList [PA (LI [Text t])]) : stack) input
--Creating List Items 
sr (Word w : AA : NewLine : PA (UList xs) : stack) input = sr (PA (LI [Word w]) : PA (UList xs) : stack) input
sr (Word w2 : PA (LI [Word w1]) : stack) input = sr (PA (LI [Text (w1 ++ " " ++ w2)]) : stack) input 
sr (Word w : PA (LI [Text t]) : stack) input = sr (PA (LI [Text (t ++ " " ++ w)]) : stack) input 
sr (NewLine : PA (LI ys) : PA (UList xs) : stack) input = sr (NewLine : PA (UList (xs ++ [PA (LI ys)])) : stack) input 
--Elements within lists
sr (NewLine : Text t : Tab : NewLine : NewLine : PA (UList xs) : stack) input = sr (PA (UList (xs ++ [PA (P [Text t])])) : stack) input 
sr (NewLine : PA p : Tab : NewLine : NewLine : PA (UList xs) : stack) input = sr (PA (UList (xs ++ [PA p])) : stack) input
sr (NewLine : PA p : Tab : NewLine : PA (UList xs) : stack) input = sr (NewLine : PA (UList (xs ++ [PA p])) : stack) input 


--Handing Paragraph Tags
sr (Text t : NewLine : NewLine : stack) input = sr (PA (P [Text t]) : NewLine : NewLine : stack) input
sr (Word w : NewLine : NewLine : stack) input = sr (PA (P [Word w]) : NewLine : NewLine : stack) input
sr (Text t : PA (P x) : stack) input = sr (PA (P (x ++ [Text t])) : stack) input
sr (Text t : NewLine : PA (P x) : stack) input = sr (PA (P (x ++ [Text t])) : stack) input
sr (Word w : PA (P x) : stack) input = sr (PA (P (x ++ [Word w])) : stack) input
sr (Word w : NewLine : PA (P x) : stack) input = sr (PA (P (x ++ [Word w])) : stack) input
sr (PA (Bold x) : NewLine : NewLine : stack) input = sr (PA (P [PA (Bold x)]) : NewLine : NewLine : stack) input
sr (PA (EM x) : NewLine : NewLine : stack) input = sr (PA (P [PA (EM x)]) : NewLine : NewLine : stack) input
sr (PA BR : PA (P (x)) : stack) input = sr (PA (P (x ++ [PA BR])) : stack) input

--Handling overall/HTML Tag
sr (RHTML : stack) input = sr (HTML [] : stack) input
sr (LHTML : stack) input = sr (stack) input
sr (HTML xs : PA x : stack) input = sr (HTML (PA x:xs) : stack) input 
sr (HTML xs : NewLine : stack) input = sr (HTML (NewLine : xs) : stack) input
sr (HTML xs : Tab : stack) input = sr (HTML (Tab : xs) : stack) input
--sr (HTML xs : Text t : stack) input = sr (HTML (Text t : xs) : stack) input
--sr (HTML xs : PA t : NewLine : PA (H1 x) : stack) input = sr (HTML (PA (H1 x) : PA t : xs) : stack) input

--Stack operations
sr stack    (i:input) = sr (i:stack) input 
sr stack [] = stack 

-- srl :: [Exp] -> [Exp] -> [Exp]
-- srl 

dfw :: String -> String 
dfw (x:xs)  | x == ' ' = xs 
            | otherwise = dfw xs 

lexer :: String -> [Exp] 
lexer s = map classify (words (preproc s)) 

parser :: [Exp] -> HTML  
parser input = case sr [] (LHTML : input ++ [RHTML]) of
    [HTML e] -> e
    ps -> error ("No parser:" ++ show ps)

convert ::  HTML -> String 
convert [] = ""
convert (NewLine : xs) = "\n" ++ convert xs
convert (Tab : xs) = "\t" ++ convert xs
convert (Word w : xs) = w ++ " " ++ convert xs
convert (Text t : xs) = t ++ convert xs
convert (PA BR : xs) = "<br>" ++ convert xs
convert (PA (Bold b) : xs) = "<strong>" ++ convert b ++ "</strong>" ++ convert xs
convert (PA (EM e) : xs) = "<em>" ++ convert e ++ "</em>" ++ convert xs
convert (PA (H1 x) : xs) = "<h1>" ++ convert x ++ "</h1>" ++ convert xs
convert (PA (H2 x) : xs) = "<h2>" ++ convert x ++ "</h2>" ++ convert xs
convert (PA (H3 x) : xs) = "<h3>" ++ convert x ++ "</h3>" ++ convert xs
convert (PA (H4 x) : xs) = "<h4>" ++ convert x ++ "</h4>" ++ convert xs
convert (PA (H5 x) : xs) = "<h5>" ++ convert x ++ "</h5>" ++ convert xs
convert (PA (H6 x) : xs) = "<h6>" ++ convert x ++ "</h6>" ++ convert xs
convert (PA (P x) : xs) = "<p>" ++ convert x ++ "</p>\n" ++ convert xs
convert (PA (Block x) : xs) = "<blockquote>" ++ convert x ++ "</blockquote>\n" ++ convert xs
convert (PA (OList xs) : xss) = "<ol>\n" ++ convert xs ++ "</ol>\n" ++ convert xss 
convert (PA (UList xs) : xss) = "<ul>\n" ++ convert xs ++ "</ul>\n" ++ convert xss
convert (PA (LI x) : xs) = "<li>" ++ convert x ++ "</li>\n" ++ convert xs

main :: IO ()
main = do 
    putStrLn "Enter file name without extension: "
    filename <- getLine 
    contents <- readFile (filename ++ ".md")
    let analyzed = lexer contents 
    print analyzed
    -- putStrLn "Here is the result of lexical analysis: "
    -- putStrLn (show analyzed)
    -- putStrLn "-------------------------------"
    let parsed = parser analyzed 
    -- putStrLn "Here is the result of parsing: "
    -- putStrLn (show parsed)
    -- let program = createProgram parsed
    --print parsed
    let html = convert parsed 
    writeFile (filename ++ ".html") html
    --putStrLn (show parsed)