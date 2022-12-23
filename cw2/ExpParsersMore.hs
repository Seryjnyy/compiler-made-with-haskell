module ExpParsersMore(minExp) where

import Parsers ( Parsers(..), parse )
import ExpressionAST ( UnOperator(..), BinOperator(..), AST(..) )
import FunParsers ( symbol, integer, identifierP )
import Control.Applicative ( Alternative(..) )



minExp :: Parsers AST
minExp = pr

pr :: Parsers AST
pr = pr' id

pr' :: (AST -> AST) -> Parsers AST
pr' f = do x <- getTerm
           do symbol "+"
              pr' (BinOp Addition (f x))
              <|>
              do symbol "-"
                 pr' (BinOp Subtraction (f x))
              <|>
              do symbol "<"
                 pr' (BinOp Less (f x))
              <|>
              do symbol "<="
                 pr' (BinOp LessOrEq (f x))
              <|>
              do symbol ">"
                 pr' (BinOp More (f x))
              <|>
              do symbol ">="
                 pr' (BinOp MoreOrEq (f x))
              <|>
              do symbol "=="
                 pr' (BinOp Equiv (f x))
              <|>
              do symbol "!="
                 pr' (BinOp NotEquiv (f x))
              <|>
              do symbol "&&"
                 pr' (BinOp And (f x))
              <|>
              do symbol "||"
                 pr' (BinOp Or (f x))
              <|>
              do symbol "*"
                 y <- getTerm
                 do
                  res <- nextOp
                  pr' (BinOp res (f(BinOp Multiplication x y)))
                  <|> return ((f(BinOp Multiplication x y)))
               <|>
                  do symbol "/"
                     y <- getTerm
                     res <- nextOp
                     pr' (BinOp res (f(BinOp Division x y)))
                      <|> return ((f(BinOp Division x y)))
            <|> return (f x)


nextOp = do 
   symbol "+"
   return Addition
   <|>
   do
   symbol "-"
   return Subtraction
   <|>
   do
   symbol "*"
   return Multiplication
   <|>
   do
   symbol "/"
   return Division
   <|>
   do
   symbol "<"
   return Less
   <|>
   do
   symbol "<="
   return LessOrEq
   <|>
   do
   symbol ">"
   return More
   <|>
   do
   symbol ">="
   return MoreOrEq
   <|>
   do
   symbol "=="
   return Equiv
   <|>
   do
   symbol "!="
   return NotEquiv
   <|>
   do
   symbol "&&"
   return And
   <|>
   do
   symbol "||"
   return Or

term :: Parsers AST
term = do 
   LitInteger  . toInteger <$> integer
   <|>
   do 
      symbol "-"
      UnOp Negation <$> getTerm
      <|>
      do
         symbol "!"
         UnOp ExclNegation <$> getTerm
      <|>
      do Variable <$> identifierP
         

brack :: Parsers AST
brack = do symbol "("
           y <- pr
           symbol ")"
           return y

getTerm :: Parsers AST
getTerm = brack <|> term  


-- lssP = symbol "<"
-- lssEqP = symbol "<="

-- grP = symbol ">"
-- grEqP = symbol ">="

-- eqP = symbol "=="

-- notEqP = symbol "!="

-- andP = symbol "&&"
-- orP = symbol "||"
-- -- negP = symbol "!" 

-- terFirstP = symbol "?"
-- terSecondP = symbol ":"


--               do symbol "?"
--                 exp1 <- pr
--                 symbol ":"
--                 exp2 <- pr
--                  pr' (TernOp AST AST AST Subtraction (f x))
--               <|>

-- pa :: Parsers AST
-- pa = pa' id

-- pa' :: (AST -> AST) -> Parsers AST
-- pa' f = do x <- getTerm
--            do symbol "+"
--               pa' (BinOp Addition (f x))
--               <|>
--               do symbol "-"
--                  pa' (BinOp Subtraction (f x))
--               <|>
--                   duMult x f "*" Multiplication
--                <|>
--                   duMult x f "/" Division
--             <|> return (f x)

-- duMult x f oper astType = do
--    y <- getTerm 
--    do symbol oper
--       y <- getTerm
--       do symbol "+"
--          pa' (BinOp Addition (f(BinOp astType x y)))
--          <|>
--          do symbol "-"
--             pa' (BinOp Subtraction (f(BinOp astType x y)))
--          <|>
--          do symbol "*"
--             pa' (BinOp Multiplication (f(BinOp astType x y)))
--          <|>
--          do symbol "/"
--             pa' (BinOp Division (f(BinOp astType x y)))
--       <|> return ((f(BinOp astType x y)))