module ExpressionAST(AST(..), BinOperator(..), UnOperator(..)) where

type Identifier = String

data AST = LitInteger Integer | Variable Identifier | BinOp BinOperator AST AST | UnOp UnOperator AST | TernOp AST AST AST deriving(Show)
data BinOperator = Addition | Subtraction | Multiplication | Division | Less | LessOrEq | More | MoreOrEq | Equiv | NotEquiv | And | Or deriving(Show, Eq)
data UnOperator = Negation | ExclNegation deriving(Show)