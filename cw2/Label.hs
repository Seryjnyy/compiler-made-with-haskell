module Label where

import Control.Applicative ( Alternative(..) )
import Data.Char ( isDigit, isSpace, isAlphaNum )
import Control.Monad.Identity (Identity)
import Debug.Trace

newtype StateTransformer st a = S(st -> (a, st))

app :: StateTransformer st a -> st -> (a, st)
app (S f) x = f x


instance Functor (StateTransformer st) where
-- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative (StateTransformer st) where
-- pure :: a -> ST a
    pure x = S (\s -> (x,s))
-- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S (\s ->
        let (f,s') = app stf s
            (x,s'') = app stx s' in (f x, s''))

instance Monad (StateTransformer st)where
-- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')

type LName = String

fresh :: StateTransformer Int LName
fresh = do
    a <- stState
    stUpdate (a+1)
    return ("#" ++ (show a))

stState :: StateTransformer st st
stState = S(\st -> (st, st))

stUpdate st = S(\_ -> ((), st))

-- Variable Environment

type Identifier = String
type StackAddress = Int

type VarEnv = [(Identifier, StackAddress)]




declTAM (VarDecl v) = do
    (ve, i) <- stState
    stUpdate ((v, i):ve, i+1)
    return [LOADL 0]

declTAM (VarDeclInit v e) = do
    (ve, i) <- stState
    stUpdate ((v, i):ve, i+1)
    return (expCode [] e)


expCode :: VarEnv -> AST -> [TAMInst]
expCode ve ast = interpretT ve ast

-- parse Declerations

-- start parsers for variable decleration
letP = token $ string "let"
inP = token $ string "in"
varP = token $ string "var"
assignSymbolP = token $ string ":=" 

alphaNumP = sat isAlphaNum

-- TODO - need to make sure first letter is capital
identifierP = do
   many alphaNumP

varDeclP = do
   varP
   VarDecl <$> identifierP

varDeclInitP = do
   varP
   x <- identifierP
   assignSymbolP
   VarDeclInit x <$> minExp

decleration = do varDeclInitP <|> varDeclP 

-- TODO - maybe ther is no declerations, it will fail if there is no declerations
declerations = do 
   x <- decleration
   do
      symbol ";"
      xs <- declerations
      return (x:xs)
     <|> return [x]


-- declPart = letP *> declerations <* inP

-- end parsers for variable decleration

-- start parsers for commands
beginP = token $ string "begin"
ifP = token $ string "if"
elseP = token $ string "else"
thenP = token $ string "then"
whileP = token $ string "while"
doP = token $ string "do"
endP = token $ string "end"
printintP = token $ string "printint"
getintP = token $ string "getint"

-- commandPart = beginP *> string "temp" <* endP

command = do commBeginEnd <|> commIfElse <|> commFunction <|> commWhile <|> commAssignP

commAssignP = do
   s <- identifierP
   assignSymbolP
   Assignment s <$> minExp 


commIfElse = do
   ifP
   e <- minExp
   thenP
   c1 <- command
   elseP
   IfThenElse e c1 <$> command

commFunction = do Func <$> commPrintFunc <|> Func <$> commGetIntFunc

commPrintFunc = do
   printintP
   PrintInt <$> do symbol "(" *> minExp <* symbol ")"

commGetIntFunc = do
   getintP
   GetInt <$> do symbol "(" *> identifierP <* symbol ")"

commWhile = do
   whileP
   e <- minExp
   doP
   WhileDo e <$> command

commBeginEnd = do
   beginP *> (BeginEnd <$> commands) <* endP

-- TODO - maybe there is no commands, it will fail if there is no commands
commands = do 
   x <- command
   do
      symbol ";"
      xs <- commands
      return (x:xs)
      <|> return [x]

-- start parser for program
programP = do
   letP
   x <- declerations
   inP
   LetIn x <$> commands

-- end parser for program

-- experiment - functions for testing

getDecl s = getDecl' $ getProgram s

getDecl' (LetIn d c) = d

getProgram s = fst $ head $ parse programP s 

getComm s = getComm' $ getProgram s

getComm' (LetIn d c) = c

getDeclSet = getDecl "let var x in begin x := 2 end"
getCommSet = getComm "let var x in begin x := 2 end"
-- experiment - functions for testing


data Program = LetIn [Decleration] [Command] deriving(Show)
data Command = Assignment Identifier AST | IfThenElse AST Command Command | WhileDo AST Command | Func UserFunction | BeginEnd [Command] deriving(Show)
data UserFunction = PrintInt AST | GetInt Identifier deriving(Show)
-- end parsers for commands

-- interpret but with TAM instructions

-- currently can't do variables
interpretT :: VarEnv -> AST -> [TAMInst]
interpretT ve (BinOp op t1 t2) = (interpretT ve t1) ++ (interpretT ve t2) ++ [getBinOperT op]
interpretT ve (LitInteger a) = [LOADL (fromInteger a)]
interpretT ve (UnOp op t1) = (interpretT ve t1) ++ [getUnOperT op]
interpretT ve (Variable i) = [LOAD (findInVarEnv ve i)]


getBinOperT :: BinOperator -> TAMInst
getBinOperT Addition = ADD
getBinOperT Subtraction = SUB
getBinOperT Multiplication = MUL
getBinOperT Division = DIV

getUnOperT :: UnOperator -> TAMInst
getUnOperT Negation = NEG


dedCompiler :: [Decleration] -> StateTransformer (VarEnv, StackAddress) [TAMInst]
dedCompiler (x:xs) = do
    a <- declTAM x
    b <- dedCompiler xs
    return (a ++ b)

dedCompiler [] = return []




-- ASTs

data AST = LitInteger Integer | Variable Identifier |BinOp BinOperator AST AST | UnOp UnOperator AST deriving(Show)
data BinOperator = Addition | Subtraction | Multiplication | Division deriving(Show, Eq)
data UnOperator = Negation deriving(Show)

data Decleration = VarDecl String | VarDeclInit String AST deriving(Show)

type MTInt = Int
type Stack = [MTInt]                                     -- new commands
data TAMInst = LOADL MTInt | MUL | DIV | ADD | SUB | NEG 
 | LOAD StackAddress | STORE StackAddress | LABEL LName | JUMP LName 
 | JUMPIFZ LName | HALT | AND | OR| NOT | PUTINT | GETINT deriving(Show, Eq)

-- data Expression = LitInteger Integer | BinOp BinOperator Expression Expression | UnOp UnOperator Expression deriving(Show)


-- AST evaluator

evaluate :: AST -> Integer
evaluate (LitInteger x)   = x
evaluate (BinOp op t1 t2) = binOpEv op (evaluate t1) (evaluate t2)
evaluate (UnOp op t)      = unOpEv op (evaluate t)

binOpEv :: BinOperator -> Integer -> Integer -> Integer
binOpEv Addition       = (+)
binOpEv Subtraction    = (-)
binOpEv Multiplication = (*)
binOpEv Division       = div

unOpEv :: UnOperator -> Integer -> Integer
unOpEv Negation = negate

-- code generator

interpret :: AST -> [String]
interpret (BinOp op t1 t2) = (interpret t1) ++ (interpret t2) ++ [getBinOper op]
interpret (LitInteger a) = ["LOADL " ++ show a]
interpret (UnOp op t1) = (interpret t1) ++ [getUnOper op]

getBinOper :: BinOperator -> String
getBinOper Addition = "ADD"
getBinOper Subtraction = "SUB"
getBinOper Multiplication = "MUL"
getBinOper Division = "DIV"

getUnOper :: UnOperator -> String
getUnOper Negation = "NEG"

-- write file


writeInstructions :: [String] -> IO()
writeInstructions s = writeFile "arith_expression.tam" (unlines (s)) 

-- start Generate TAM for commands
commCode ve (IfThenElse e c1 c2) = do
   let te = expCode ve e
   l1 <- fresh
   l2 <- fresh
   tc1 <- commCode ve c1
   tc2 <- commCode ve c2
   return $ te ++ [JUMPIFZ l1] ++ tc1 ++ [JUMP l2, LABEL l1] ++ tc2 ++ [LABEL l2]

commCode ve (Func (PrintInt e)) = do
   let te = expCode ve e
   return $ te ++ [PUTINT]

commCode ve (Func (GetInt l)) = do
   return $ [GETINT] ++ [STORE $ findInVarEnv ve l]

commCode ve (Assignment i e) = do
   let te = expCode ve e
   return $ te ++ [STORE $ findInVarEnv ve i]

-- could be wrong, maybe do the command like the expression
commCode ve (WhileDo e c) = do
   let te = expCode ve e
   tc <- commCode ve c
   l1 <- fresh
   l2 <- fresh
   return $ [LABEL l1] ++ te ++ [JUMPIFZ l2] ++ tc ++ [JUMP l1] ++ [LABEL l2]

commCode ve (BeginEnd (x:xs)) = do
   tc <- commCode ve x
   rest <- (commCode ve (BeginEnd xs))
   return $ tc ++ rest

commCode ve (BeginEnd []) = return []


generateCode ve c = do
   code <- commCode ve c
   return $ code ++ [HALT]

findInVarEnv [] l = 0
findInVarEnv (x:xs) l = if (fst x) == l then snd x else findInVarEnv xs l


-- find location of variable, stack address
-- Expression, then STORE

-- end Generate TAM for commands


-- start Virtual machine
final instructions = execTAM [] instructions

tokenise :: [String] -> [TAMInst]
tokenise [] = []
tokenise (x:xs) = makeToken x : tokenise xs 

getLoadInst :: String -> TAMInst
getLoadInst s = LOADL (read b)
        where b = last (words s)

makeToken :: String -> TAMInst
makeToken s
        | s == "ADD" = ADD
        | s == "SUB" = SUB
        | s == "MUL" = MUL
        | s == "DIV" = DIV
        | s == "NEG" = NEG
        | (take 3 s) == "LOA" = getLoadInst s

execute :: Stack -> TAMInst -> Stack
execute stk (LOADL a) = a : stk
execute stk (LOAD a) = (stk!!((length stk) - 1 - a)):stk
execute (x:xs) (STORE a) = (take ((length xs) - 1 - a) xs) ++ [x] ++ (drop ((length xs) - a) xs)
execute stk (LABEL l) = stk -- can't do much because jump needs the program to be in memory so need the state transfer
execute (x:y:stk) ADD = y+x : stk
execute (x:y:stk) SUB = y-x : stk
execute (x:y:stk) MUL = y*x : stk
execute (x:y:stk) DIV = y `div` x : stk
execute (x:stk)   NEG = (-x) : stk

-- needs to get result and run it, the executeNew should handle the setting of the next instruction
-- only stop when halt is reached

actuallExec TAMState{tsStack, tsCounter, tsCode} = if tsCounter /= (-1) then executeNew (tsCode!!tsCounter) TAMState{tsStack = tsStack, tsCounter = tsCounter, tsCode = tsCode} else TAMState{tsStack = tsStack, tsCounter = tsCounter, tsCode = tsCode} 


executeNew :: TAMInst -> TAMState -> TAMState
executeNew (LOADL a) TAMState{tsStack, tsCounter, tsCode} = actuallExec TAMState{tsStack = a:tsStack, tsCounter = tsCounter+1, tsCode = tsCode}
executeNew (LOAD a) TAMState{tsStack, tsCounter, tsCode} = actuallExec TAMState{tsStack = tsStack!!((length tsStack) - 1 - a):tsStack, tsCounter = tsCounter+1, tsCode = tsCode}
executeNew (STORE a) TAMState{tsStack, tsCounter, tsCode}  = actuallExec TAMState{tsStack = insertValueAtIndex tsStack a, tsCounter = tsCounter+1, tsCode = tsCode}
executeNew (LABEL l) TAMState{tsStack, tsCounter, tsCode}  = actuallExec TAMState{tsStack = tsStack, tsCounter = tsCounter + 1, tsCode = tsCode}
executeNew (JUMP l) TAMState{tsStack, tsCounter, tsCode}  = actuallExec TAMState{tsStack = tsStack, tsCounter = findInList tsCode (LABEL l) 0, tsCode = tsCode}
executeNew (JUMPIFZ l) TAMState{tsStack, tsCounter, tsCode}  = actuallExec TAMState{tsStack = popTop tsStack, tsCounter = if checkTop tsStack then findInList tsCode (LABEL l) 0 else tsCounter + 1, tsCode = tsCode}
executeNew HALT TAMState{tsStack, tsCounter, tsCode} = actuallExec TAMState{tsStack = tsStack, tsCounter = -1, tsCode = tsCode}

executeNew ADD TAMState{tsStack, tsCounter, tsCode}  = actuallExec TAMState{tsStack = stkAdd tsStack, tsCounter = tsCounter + 1, tsCode = tsCode}
executeNew SUB TAMState{tsStack, tsCounter, tsCode}  = actuallExec TAMState{tsStack = stkSub tsStack, tsCounter = tsCounter + 1, tsCode = tsCode}
executeNew MUL TAMState{tsStack, tsCounter, tsCode}  = actuallExec TAMState{tsStack = stkMul tsStack, tsCounter = tsCounter + 1, tsCode = tsCode}
executeNew DIV TAMState{tsStack, tsCounter, tsCode}  = actuallExec TAMState{tsStack = stkDiv tsStack, tsCounter = tsCounter + 1, tsCode = tsCode}
executeNew NEG TAMState{tsStack, tsCounter, tsCode}  = actuallExec TAMState{tsStack = stkNeg tsStack, tsCounter = tsCounter + 1, tsCode = tsCode}

executeNew PUTINT TAMState{tsStack, tsCounter, tsCode}  = actuallExec TAMState{tsStack = popAndPrint tsStack, tsCounter = tsCounter + 1, tsCode = tsCode}
executeNew GETINT TAMState{tsStack, tsCounter, tsCode}  = actuallExec TAMState{tsStack = readAndAdd tsStack, tsCounter = tsCounter + 1, tsCode = tsCode}

-- TODO - not finished
popAndPrint (x:xs) = xs
readAndAdd xs = (9999:xs)

stkAdd (x:y:stk) = y+x : stk
stkSub (x:y:stk) = y-x : stk
stkMul (x:y:stk) = y*x : stk
stkDiv (x:y:stk) = y `div` x : stk
stkNeg (x:stk)   = (-x) : stk

-- for JUMPIFZ
popTop (x:xs) = xs

-- not sure about these could be wrong
-- executeNew (AND) TAMState{tsStack, tsCounter, tsCode} = TAMState{tsStack = tsStack, tsCounter = tsCounter+1, tsCode = tsCode}
-- OR
-- NOT
checkAnd (x:y:xs) = (x /= 0) && (y /= 0) 
checkOr (x:y:xs) =  (x /= 0) || (y /= 0) 
-- doNot (x:xs) = if x == 0 then 1 

checkTop (x:xs) = x == 0
checkTop [] = False
-- go through code list, and find label with LName, set program counter to that index
findInList [] l i = i
findInList (x:xs) l i = if x == l then i else findInList xs l (i+1)

insertValueAtIndex (x:xs) a = (take ((length xs) - 1 - a) xs) ++ [x] ++ (drop ((length xs) - a) xs)

data TAMState = TAMState {
   tsCode :: [TAMInst],
   tsCounter :: Int,
   tsStack :: Stack
} deriving(Show)

-- exec :: TAMInst -> TAMState -> TAMState
exec TAMState{tsCounter} = TAMState{tsCounter = (tsCounter + 1), tsCode = [], tsStack = []}

incrementCounter TAMState{tsCounter, tsCode, tsStack} = TAMState{tsCounter = tsCounter + 1, tsCode = tsCode, tsStack = tsStack} 
setCounter TAMState{tsCounter, tsCode, tsStack} x = TAMState{tsCounter = x, tsCode = tsCode, tsStack = tsStack} 

startExec = incrementCounter TAMState {tsCode = [LOAD 2], tsCounter = 1, tsStack = []}
-- take LENGTH - 1 - n ++ value ++ drop LENGTH - n 

execTAM :: Stack -> [TAMInst] -> Stack
execTAM = foldl execute

-- end Virtual machine


-- Fun parsers


item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

char :: Char -> Parser Char
char x = sat (== x)



string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

int :: Parser Int
int = do 
         symbol "-"
         n <- nat
         return (-n)
       <|> nat

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

symbol :: String -> Parser String
symbol xs = token (string xs) 
integer = token nat 
natural = token nat

digit :: Parser Char
digit = sat isDigit

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x 

-- parser


newtype Parser a = P(String -> [(a, String)])

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

parse (P p) inp = p inp

-- start Exp parsers


minExp :: Parser AST
minExp = pr

pr :: Parser AST
pr = pr' id

pr' :: (AST -> AST) -> Parser AST
pr' f = do x <- getTerm
           do symbol "+"
              pr' (BinOp Addition (f x))
              <|>
              do symbol "-"
                 pr' (BinOp Subtraction (f x))
              <|>
              do symbol "*"
                 y <- getTerm
                 do symbol "+"
                    pr' (BinOp Addition (f(BinOp Multiplication x y)))
                    <|>
                    do symbol "-"
                       pr' (BinOp Subtraction (f(BinOp Multiplication x y)))
                    <|>
                    do symbol "*"
                       pr' (BinOp Multiplication (f(BinOp Multiplication x y)))
                    <|>
                    do symbol "/"
                       pr' (BinOp Division (f(BinOp Multiplication x y)))
                    <|> return ((f(BinOp Multiplication x y)))
               <|>
                  do symbol "/"
                     y <- getTerm
                     do symbol "+"
                        pr' (BinOp Addition (f(BinOp Division x y)))
                        <|>
                        do symbol "-"
                           pr' (BinOp Subtraction (f(BinOp Division x y)))
                        <|>
                        do symbol "*"
                           pr' (BinOp Multiplication (f(BinOp Division x y)))
                        <|>
                        do symbol "/"
                           pr' (BinOp Division (f(BinOp Division x y)))
                        <|> return ((f(BinOp Division x y)))
            <|> return (f x)

term :: Parser AST
term = do x <- integer 
          return (LitInteger (toInteger x))
          <|>
          do symbol "-"
             y <- getTerm
             return (UnOp Negation y)
         <|>
         do var <- identifierP
            return (Variable var)

brack :: Parser AST
brack = do symbol "("
           y <- pr
           symbol ")"
           return y

getTerm :: Parser AST
getTerm = term <|> brack

-- end Exp parsers