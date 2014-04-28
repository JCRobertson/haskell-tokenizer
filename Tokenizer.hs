module Tokenizer where

import Data.Char
import Data.List.Split

data Token = Begin | End
           | If | Fi | Else
           | For | From | To | While | Break | Continue
           | Print
           | T | F
            
           | SemiColon | Equal
           | BraceOpen | BraceClose | ParenOpen | ParenClose
           | OpOr | OpAnd | OpNot | OpL | OpLE | OpG | OpGE | OpEq | OpNeq
           | OpAdd | OpSub | OpMul | OpDiv | OpPow
            
           | Ident String
           | Str String
           | Num Int
           deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize s =  fetchTokens (wordTokens (seperateStrings (removeSingleLine False False (removeBlocks 0 False False s))))

    
{- nesting level, in string, in line comment -}
removeBlocks :: Int -> Bool -> Bool -> String -> String
removeBlocks nest inst inl [] = ""

removeBlocks nest inst inl (x:[]) = x:[]

removeBlocks nest inst inl (x:y:[]) | nest == 1 && x == '*' && y == '/' = []
                                    | nest > 0 && (x /= '*' || y /= '/') = error "Block comment not closed"
                                    | x == '/' && y == '*' = error "Block comment opens at end of string"
                                    | otherwise = x:y:[]

removeBlocks nest inst inl (x:y:xs) | nest == 0 && inl == False && inst == True && x == '\\' && y == '"' = x : y : removeBlocks nest True False xs
                                    | nest == 0 && inl == False && x == '"' = x : removeBlocks nest (not inst) False (y:xs)
                                    | nest == 0 && inst == False && x == '/' && y == '/' =  x : y : removeBlocks nest False True xs
                                    | nest == 0 && inst == False && inl == True && x == '\n' =  x : removeBlocks nest False False (y:xs)  
                                    | inst == False && inl == False && x == '/' && y == '*' = removeBlocks (nest+1) False False xs 
                                    | nest > 0 && x == '*' && y == '/' = removeBlocks (nest - 1) False False xs 
                                    | nest > 0 = removeBlocks nest False False (y:xs)  
                                    | otherwise = x : removeBlocks nest inst inl (y:xs)  


removeSingleLine :: Bool -> Bool -> String -> String
removeSingleLine inl inst [] = ""

removeSingleLine inl inst (x:[])    {--| inst == True && inl == False && x /= '"' = error "String not closed" --}
                                    | inl == True = []
                                    | otherwise = [x]

removeSingleLine inl inst (x:y:xs)  | x == '"' && inl == False = x : removeSingleLine False (not inst) (y:xs)
                                    | inst == False && x == '/' && y == '/' =  removeSingleLine True False xs
                                    | inst == False && x == '\n' && inl == True =  removeSingleLine False False (y:xs)
                                    | inst == False && inl == True = removeSingleLine True False (y:xs)
                                    | otherwise = x : removeSingleLine False inst (y:xs)



seperateStrings :: String -> [String]      
seperateStrings s = concatStrings (split (whenElt (== '"')) s)

concatStrings :: [String] -> [String]
concatStrings [] = []
concatStrings (x:[]) = [x]
concatStrings (x:y:[]) = (x:y:[])
concatStrings (x:y:z:xs)      | (last x) == '\\' =  concatStrings ((x ++ y ++ z):xs)
                              | x == "\"" =  concatStrings ((x ++ y):z:xs) 
                              | (head x) == '"' && y == "\"" = (x ++ y) : concatStrings (z:xs)
                              | otherwise = x : concatStrings (y:z:xs)
          
{- Apply word function to everything that is not a string -}                    
wordTokens :: [String] -> [String]
wordTokens [] = []
wordTokens (x:xs)   | (head x) /= '"' = words x ++ wordTokens xs
                    | otherwise = x : wordTokens xs 

fetchTokens:: [String] -> [Token]
fetchTokens [] = []
fetchTokens (x:xs)  | (head x) == '"' = (Str (tail(init x))) : fetchTokens xs
                    | checkNum x == True = (Num (getNum x)) : fetchTokens xs
                    | checkIdent (checkToken x) == True = confirmIdent x ++ fetchTokens xs
                    | otherwise = (checkToken x) : fetchTokens xs

checkToken :: String -> Token
checkToken x = 
    case x of
      "BEGIN"   ->  Begin
      "END"     ->  End
      "if"      ->  If
      "fi"      ->  Fi
      "else"    ->  Else
      "for"     ->  For
      "from"    ->  From
      "to"      ->  To
      "while"   ->  While
      "break"   ->  Break
      "continue" ->  Continue
      "print"   ->  Print
      "true"    ->  T
      "false"   ->  F
      ";"       ->  SemiColon
      "{"       ->  BraceOpen
      "}"       ->  BraceClose
      "("       ->  ParenOpen
      ")"       ->  ParenClose
      "||"      ->  OpOr
      "&&"      ->  OpAnd
      "!"       ->  OpNot      
      "<="      ->  OpLE
      ">="      ->  OpGE
      "=="      ->  OpEq
      "="       ->  Equal
      "<"       ->  OpL
      ">"       ->  OpG
      "!="      ->  OpNeq
      "+"       ->  OpAdd
      "-"       ->  OpSub 
      "/"       ->  OpDiv
      "**"      ->  OpPow
      "*"       ->  OpMul
      _         ->  (Ident x)

checkOperator :: String -> Token
checkOperator x = 
    case x of
      ";"       ->  SemiColon
      "{"       ->  BraceOpen
      "}"       ->  BraceClose
      "("       ->  ParenOpen
      ")"       ->  ParenClose
      "||"      ->  OpOr
      "&&"      ->  OpAnd
      "!"       ->  OpNot      
      "<="      ->  OpLE
      ">="      ->  OpGE
      "=="      ->  OpEq
      "="       ->  Equal
      "<"       ->  OpL
      ">"       ->  OpG
      "!="      ->  OpNeq
      "+"       ->  OpAdd
      "-"       ->  OpSub 
      "/"       ->  OpDiv
      "**"      ->  OpPow
      "*"       ->  OpMul
      _         ->  (Ident x)

checkNum :: String -> Bool
checkNum [] = True
checkNum (x:xs) = isDigit x && checkNum xs

checkIdent :: Token -> Bool
checkIdent (Ident _) = True
checkIdent _ = False

getNum :: String -> Int
getNum [] = error "Empty Number"
getNum [x] = ord(x) - 48
getNum (xs) = 10*(getNum ((init xs))) + ((ord (last xs))-48)

confirmIdent :: String -> [Token]
confirmIdent x = fetchTokens2 (words (spaceIdent x))

spaceIdent :: String -> String
spaceIdent [] = []
spaceIdent [x]  | checkIdent (checkToken [x]) /= True = " " ++ [x]
                | otherwise = [x]
spaceIdent (x:y:xs) | checkIdent (checkOperator (x:[y])) /= True = " " ++ [x] ++ [y] ++ " " ++ spaceIdent xs
                    | checkIdent (checkOperator [x]) /= True = " " ++ [x] ++ " " ++ spaceIdent (y:xs)
                    | otherwise = x : spaceIdent (y:xs)

fetchTokens2 :: [String] -> [Token]
fetchTokens2 [] = []
fetchTokens2 (x:xs) | checkIdent (checkToken x) == True && checkNum x == True = (Num (getNum x)) : fetchTokens2 xs 
                    | otherwise = (checkToken x) : fetchTokens2 xs

showTokens :: [Token] -> String
showTokens [] = ""
showTokens [x] = unpackTokens x
showTokens (x:xs) = unpackTokens x ++ showTokens xs

unpackTokens :: Token -> String
unpackTokens (Str x)   = "String " ++ x ++ "\n"
unpackTokens (Num x)   = "Num " ++ (show x) ++ "\n"
unpackTokens (Ident x)   = "Ident " ++ x ++ "\n"
unpackTokens Begin = "Begin\n"
unpackTokens End = "End\n"
unpackTokens If = "If\n"
unpackTokens Fi = "Fi\n"
unpackTokens Else = "Else\n"
unpackTokens From = "From\n"
unpackTokens For = "For\n"
unpackTokens To = "To\n"
unpackTokens While = "While\n"
unpackTokens Break = "Break\n"
unpackTokens Continue = "Continue\n"
unpackTokens Print = "Print\n"
unpackTokens T = "True\n"
unpackTokens F = "False\n"
unpackTokens SemiColon = "SemiColon\n"
unpackTokens BraceOpen = "BraceOpen\n"
unpackTokens BraceClose = "BraceClose\n"
unpackTokens ParenOpen = "ParenOpen\n"
unpackTokens ParenClose = "ParenClose\n"
unpackTokens OpOr = "OpOr\n"
unpackTokens OpAnd = "OpAnd\n"
unpackTokens OpNot = "OpNot\n"
unpackTokens OpLE = "OpLE\n"
unpackTokens OpGE = "OpGE\n"
unpackTokens OpEq = "OpEq\n"
unpackTokens Equal = "Equal\n"
unpackTokens OpL = "OpL\n"
unpackTokens OpG = "OpG\n"
unpackTokens OpNeq = "OpNeq\n"
unpackTokens OpAdd = "OpAdd\n"
unpackTokens OpSub = "OpSub\n"
unpackTokens OpDiv = "OpDiv\n"
unpackTokens OpPow = "OpPow\n"
unpackTokens OpMul = "OpMul\n"

