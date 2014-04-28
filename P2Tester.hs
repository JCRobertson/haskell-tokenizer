module P2Tester where

import Tokenizer
import Control.Monad.State
import Debug.Trace(trace)

t_tests :: [(Int,String,[Token])]
t_tests = [
-------------------------------------------------------------------------------------------------------
-- COMMENT OUT any line that crashes your program; final score's numerator will account for the missed test case.
-------------------------------------------------------------------------------------------------------
    -- each keyword, once
    (4,"BEGIN END",	[Begin,End]),
    (4,"if fi else for from to while break continue",[If,Fi,Else,For,From,To,While,Break,Continue]),
    (3,"print true false",[Print,T,F]),
    -- non-keyword identifiers that contain keywords.
    (5,"begin Beginning total continued awhiler awhile",[Ident "begin",Ident "Beginning",Ident "total",Ident "continued",Ident "awhiler",Ident "awhile"]),
    -- each operator; first, the ones that aren't prefixes of others.
    (5,"; = { } ( ) || && ! < > + - * /",[SemiColon,Equal,BraceOpen,BraceClose,ParenOpen,ParenClose,OpOr,OpAnd,OpNot,OpL,OpG,OpAdd,OpSub,OpMul,OpDiv]),
    -- and then all the ones that have other operators as prefixes
    (5,"== = != ! ** * <= < >= >",[OpEq,Equal,OpNeq,OpNot,OpPow,OpMul,OpLE,OpL,OpGE,OpG]),
    -- identifiers
    (4,"once UPON a tIMe",[Ident "once",Ident "UPON",Ident "a",Ident "tIMe"]),
    (3,"a b c\td\nefg",[Ident "a",Ident "b",Ident "c",Ident "d",Ident "efg"]),
    (3,"BEGIN first = second;\nthird = fourth; END",[Begin,Ident "first",Equal,Ident "second",SemiColon,Ident "third",Equal,Ident "fourth",SemiColon,End]),
    -- numbers
    (2,"0",[Num 0]),
    (4,"1 2 3 4 5 6 7 8 9",[Num 1,Num 2,Num 3,Num 4,Num 5,Num 6,Num 7,Num 8,Num 9]),
    (4,"10 11 12 13 14 15 16 17 18 19 20 300 401 12341234",[Num 10,Num 11,Num 12,Num 13,Num 14,Num 15,Num 16,Num 17,Num 18,Num 19,Num 20,Num 300,Num 401,Num 12341234]),
    -- strings
    (3,"BEGIN\n x = \"simple string\" ; END",[Begin,Ident "x",Equal,Str "simple string",SemiColon,End]),
    (2,"BEGIN\n x = \"contains \\\"inner quotes \\\" here.\" ; END",[Begin,Ident "x",Equal,Str "contains \\\"inner quotes \\\" here.",SemiColon,End]),
    (2,"BEGIN\n x = \" contains for if to from keywords\" ; END ",[Begin,Ident "x",Equal,Str " contains for if to from keywords",SemiColon,End]),
    (2,"BEGIN\n x = \"escapes inside: \\n \\t \\\" \\' done\" ; END",[Begin,Ident "x",Equal,Str "escapes inside: \\n \\t \\\" \\' done",SemiColon,End]),
    (2,"BEGIN\n x = \"comments inside: pythons uses // for int division.\" ; END",[Begin,Ident "x",Equal,Str "comments inside: pythons uses // for int division.",SemiColon,End]),
    (2,"BEGIN\n x = \"a multiline comment /* looks like */ that.\" ; END",[Begin,Ident "x",Equal,Str "a multiline comment /* looks like */ that.",SemiColon,End] ),
    (2,"BEGIN\n x = \"open: /*\";\n y = \"close */ done\";\nEND",[Begin,Ident "x",Equal,Str "open: /*",SemiColon,Ident "y",Equal,Str "close */ done",SemiColon,End]),
    -- comments
    (3,"// garbage\nBEGIN END",[Begin,End]),
    (2,"BEGIN //garbage\nEND",[Begin,End]),
    (2,"BEGIN //garbage\n\n\n//more garbage\nEND//done",[Begin,End]),
    (2,"BEGIN /* garbage */ END",[Begin,End]),
    (1,"BEGIN /* garbage\n garb\nage*/ END",[Begin,End]),
    (2,"BEGIN /* one /* two /* three */ two */ one */ END",[Begin,End]),
    (1,"BEGIN /* one /* two // /* \nthree // */\n two */\n one */ END",[Begin,End]),
    -- adjacent operators and identifiers
    (2,"BEGIN\n x=1+2;\n y=0;\n for(i from 1 to 10){ y=y+x;}\nprint(y);\n END ",[Begin,Ident "x",Equal,Num 1,OpAdd,Num 2,SemiColon,Ident "y",Equal,Num 0,SemiColon,For,ParenOpen,Ident "i",From,Num 1,To,Num 10,ParenClose,BraceOpen,Ident "y",Equal,Ident "y",OpAdd,Ident "x",SemiColon,BraceClose,Print,ParenOpen,Ident "y",ParenClose,SemiColon,End]),
    -- strings.comments
    (2,"BEGIN\n  x = \"strings are more important than comments.\";\n  y = \"for instance, // this should be a string only\";\n  z = \"similarly, /* this part\";\n  w = \" and this part*/ are in two separate strings\";\nEND\n",[Begin,Ident "x",Equal,Str "strings are more important than comments.",SemiColon,Ident "y",Equal,Str "for instance, // this should be a string only",SemiColon,Ident "z",Equal,Str "similarly, /* this part",SemiColon,Ident "w",Equal,Str " and this part*/ are in two separate strings",SemiColon,End]),
    -- strings.escapes
    (3,"BEGIN\n  x = \"escapes include \\n,\\t, \\\",\\'\";\n  y = \"escapes are typed as \\\\\\n, \\\\\\t, \\\\\\\",\\\\\\'\";\nEND\n",[Begin,Ident "x",Equal,Str "escapes include \\n,\\t, \\\",\\'",SemiColon,Ident "y",Equal,Str "escapes are typed as \\\\\\n, \\\\\\t, \\\\\\\",\\\\\\'",SemiColon,End]),
    -- div360
    (3,"/*\n * this program uses our pitifully under-empowered language, \n * CLANG0, to print out all divisors of 360. */\nBEGIN\n  n = 360;\n\n  print (\"divisors of \" + n + \":\");\n\n  for (i from 1 to (n-1)) {\n    temp = n;\n    \n    // poor-man's modulus.\n    while ( temp >= i) {\n      temp = temp - i;\n    }\n    // did we divide evenly?\n    if ( temp == 0 ) {\n      print (i);\n    }\n    fi\n  }\n  print (\"all done, you \\\"serious\\\" code watcher, you.\")\n  \n  \nEND\n",[Begin,Ident "n",Equal,Num 360,SemiColon,Print,ParenOpen,Str "divisors of ",OpAdd,Ident "n",OpAdd,Str ":",ParenClose,SemiColon,For,ParenOpen,Ident "i",From,Num 1,To,ParenOpen,Ident "n",OpSub,Num 1,ParenClose,ParenClose,BraceOpen,Ident "temp",Equal,Ident "n",SemiColon,While,ParenOpen,Ident "temp",OpGE,Ident "i",ParenClose,BraceOpen,Ident "temp",Equal,Ident "temp",OpSub,Ident "i",SemiColon,BraceClose,If,ParenOpen,Ident "temp",OpEq,Num 0,ParenClose,BraceOpen,Print,ParenOpen,Ident "i",ParenClose,SemiColon,BraceClose,Fi,BraceClose,Print,ParenOpen,Str "all done, you \\\"serious\\\" code watcher, you.",ParenClose,End]),
    (1,"",[])
    ]

st_tests :: [(Int,[Token],String)]
st_tests = [
    (2, [Begin,End,If,Fi,Else,For,From,To,While,Break,Continue,Print],"Begin\nEnd\nIf\nFi\nElse\nFor\nFrom\nTo\nWhile\nBreak\nContinue\nPrint\n"),
    (1, [T,F],"True\nFalse\n"),
    (2, [Ident "abc", Ident "def", Ident "IDENT"],"Ident abc\nIdent def\nIdent IDENT\n"),
    (2, [SemiColon,Equal,BraceOpen,BraceClose,ParenOpen,ParenClose],"SemiColon\nEqual\nBraceOpen\nBraceClose\nParenOpen\nParenClose\n"),
    (2, [OpOr,OpAnd,OpNot,OpL,OpG,OpAdd,OpSub,OpMul,OpDiv,OpEq,Equal,OpNeq,OpLE,OpL,OpGE,OpG],"OpOr\nOpAnd\nOpNot\nOpL\nOpG\nOpAdd\nOpSub\nOpMul\nOpDiv\nOpEq\nEqual\nOpNeq\nOpLE\nOpL\nOpGE\nOpG\n"),
    (1, [Num 0, Num 1, Num 2, Num 100, Num 918273645],"Num 0\nNum 1\nNum 2\nNum 100\nNum 918273645\n"),
    (2, [Str "abcde"],"String abcde\n"),
    (2, [Str "a\\nb\\tc\\\"word\\\"d"],"String a\\nb\\tc\\\"word\\\"d\n"),
    (1, [],"")
    ]

display fn = readFile fn >>= putStrLn . show


prepare f trips = map (\(testnum,(pts,inn,out))->(testnum,pts,inn,out,f inn)) $ zip [0..] trips

test_all = let  -- build up all data for tokenize test.
--------------------------------------------------------------------------------
-- SECOND VERSION helps if your code had assumed we always had a bit more input left over somewhere, such as after a number.
                t_data = prepare tokenize t_tests
--                t_data = prepare tokenize $ map (\(a,b,c)-> (a,b++"\n",c)) t_tests
--------------------------------------------------------------------------------
                -- perform scoring on these outcomes.
                (t_textout, t_score) = runState (go_t t_data) 0
                max_tokenize_score = sum (map (\(a,b,c)->a) t_tests)
                
                -- prepare all details for testing showToken.
                st_data = prepare showTokens st_tests
                -- score the results.
                (st_textout,st_score) = runState (go_st st_data) 0
                max_st_score = sum (map (\(a,b,c)->a) st_tests)
            in putStr $
             "\n\n===== testing tokenize =====\n"  ++  t_textout ++ "\ttokenize score: "  ++show t_score ++"/"++show max_tokenize_score++"\n\n"
               ++"===== testing showTokens =====\n"++ st_textout ++ "\tshowTokens score: "++show st_score++"/"++show max_st_score++"\n\n"
               ++ "TOTAL SCORE: "++ show (t_score + st_score) ++ "/"++show ({-max_tokenize_score + max_st_score-}100)++"\n\n"
    where
        go_t [] = return ""
        go_t ((number,points,inn,out,expected):rs) = do
            earned <- get
            let correct = out==expected
            put $ (if correct then points else 0)+earned
--------------------------------------------------------------------------------
-- SECOND VERSION is more verbose, but occasionally would crash someone's code.
            let stringout = ("    #"++show number++": ") ++ (if correct then "√ " else "WRONG: <"++show inn++">")++"\n"
            let stringout = ("    #"++show number++": ") ++ (if correct then "√ " else "WRONG: <"++show inn++">"++"\n\texpected: "++show expected++"\n\treceived: "++show out)++"\n"
--------------------------------------------------------------------------------
            rest <- (go_t rs)
            return $ stringout ++ rest
        go_st [] = return ""
        go_st ((testnum,pointsPossible,inn,out,result):rs) = do
        	let correct = out==result
        	modify $ \s-> s+(if correct then pointsPossible else 0)
        	let stringout = ("    #"++show testnum++": ") ++ (if correct then "√ " else "WRONG: "++show inn++"")++"\n"
        	rest <- go_st rs
        	return $ stringout ++ rest


