module CommandParser(commands) where

import FunParsers(token, string, symbol, identifierP, assignSymbolP)
import Control.Applicative ( Alternative(..) )
import ExpParsers(minExp)
import CommandAST(Command(..), UserFunction(..))

beginP = token $ string "begin"
ifP = token $ string "if"
elseP = token $ string "else"
thenP = token $ string "then"
whileP = token $ string "while"
doP = token $ string "do"
endP = token $ string "end"
printintP = token $ string "printint"
getintP = token $ string "getint"


-- TODO - maybe there is no commands, it will fail if there is no commands
commands = do 
   x <- command
   do
      symbol ";"
      xs <- commands
      return (x:xs)
      <|> return [x]
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

