{-
    William Kepley
-}

data Tag =  H1 [Exp] | H2 [Exp] | H3 [Exp] | H4 [Exp] | H5 [Exp] 
            | H6 [Exp] | Bold [Exp] | OList [Exp] | Block [Exp]
            | UList [Exp] | LI [Exp] | P [Exp] | BR | EM [Exp]
            deriving Show

data Exp =  Text String | Hash | Word String | PA Tag | Comb [Exp]
            | HTML [Exp] | LHTML | RHTML | NewLine | DA | AA | TA 
            | BB
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
classify "***" = TA 
classify "___" = TA
classify "**" = DA
classify "__" = DA
classify "*" = AA
classify "_" = AA 
classify "\\n" = NewLine 
classify "<br>" = PA (BR)
classify ">" = BB

classify s = Word s 


sr :: [Exp] -> [Exp] -> [Exp]
--Handling text
sr (NewLine : Word s : stack) input = sr (Text s : stack) input
sr (Word t : Word s : stack) input = sr (Text (s ++ " " ++ t) : stack) input
sr (Word w : Text t : stack) input = sr (Text (t ++ " " ++ w) : stack) input 
sr (PA (Bold x) : Text t : stack) input = sr (Comb [Text t, PA (Bold x)] : stack) input

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
sr (PA t : PA (H2 h) : stack) input = sr (PA (H3 (h ++ [PA t])) : stack) input

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
sr (Text t : PA (Block x) : stack) input = sr (PA (Block (x ++ [Text t])) : stack) input
sr (Word w : PA (Block [Text t]) : stack) input = sr (PA (Block [Text (t ++ " " ++ w)]) : stack) input
sr (Word w : PA (Block [Word t]) : stack) input = sr (PA (Block [Text (t ++ " " ++ w)]) : stack) input
sr (PA (Block b2) : NewLine : PA (Block b1) : stack) input = sr (PA (Block (b1 ++ b2)) : stack) input
sr (PA (Block b2) : BB : NewLine : PA (Block b1) : stack) input = sr (PA (Block (b1 ++ [PA (Block b2)])) : stack) input

--Handling overall/HTML Tag
sr (RHTML : stack) input = sr (HTML [] : stack) input
sr (LHTML : stack) input = sr (stack) input
sr (HTML xs : PA x : stack) input = sr (HTML (PA x:xs) : stack) input 
sr (HTML xs : NewLine : stack) input = sr (HTML (NewLine : xs) : stack) input
--sr (HTML xs : PA t : NewLine : PA (H1 x) : stack) input = sr (HTML (PA (H1 x) : PA t : xs) : stack) input

--Stack operations
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
convert (NewLine : xs) = "\n" ++ convert xs
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
convert (PA (P x) : xs) = "<p>" ++ convert x ++ "</p>" ++ convert xs
convert (PA (Block x) : xs) = "<blockquote>" ++ convert x ++ "</blockquote>" ++ convert xs

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