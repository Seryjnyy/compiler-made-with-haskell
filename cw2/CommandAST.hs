module CommandAST(Command(..), UserFunction(..)) where

import ExpressionAST(AST(..))
import DeclerationAST(Decleration(..))

type Identifier = String

data Command = Assignment Identifier AST | IfThenElse AST Command Command | WhileDo AST Command | Func UserFunction | BeginEnd [Command] deriving(Show)
data UserFunction = PrintInt AST | GetInt Identifier deriving(Show)