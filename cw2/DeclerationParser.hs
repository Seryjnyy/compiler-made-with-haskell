module DeclerationParser(parseDeclaration) where

import FunParsers(token, string, sat, symbol, identifierP, assignSymbolP)

import Control.Applicative ( Alternative(..) )
import DeclerationAST ( Decleration(..) )
import ExpParsers(minExp)


letP = token $ string "let"
inP = token $ string "in"
varP = token $ string "var"


varDeclP = do
   varP
   VarDecl <$> identifierP

varDeclInitP = do
   varP
   x <- identifierP
   assignSymbolP
   VarDeclInit x <$> minExp

declaration = do varDeclInitP <|> varDeclP 

-- TODO - maybe ther is no declerations, it will fail if there is no declerations
declarations = do 
   x <- declaration
   do
      symbol ";"
      xs <- declarations
      return (x:xs)
     <|> return [x]

parseDeclaration = letP *> declarations <* inP