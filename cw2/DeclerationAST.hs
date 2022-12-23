module DeclerationAST(Decleration(..)) where

import ExpressionAST(AST(..))

data Decleration = VarDecl String | VarDeclInit String AST deriving(Show)