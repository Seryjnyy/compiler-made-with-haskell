module ProgramParser(parseProgram, Program(..)) where

import DeclerationParser(parseDeclaration)
import Parsers(parse)
import CommandParser(commands)
import DeclerationAST(Decleration(..))
import CommandAST(Command(..))

data Program = LetIn [Decleration] [Command] deriving(Show)

parseProgram = do
    decl <- parseDeclaration
    comm <- commands
    return $ LetIn decl comm