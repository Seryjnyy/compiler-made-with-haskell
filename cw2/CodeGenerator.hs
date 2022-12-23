module CodeGenerator(generateCodeForProgram, writeInstructions) where

import ExpressionAST(AST(..), BinOperator(..), UnOperator(..))
import TAMVirtualMachine(TAMInst(..))
import StateTransformer ( StateTransformer, stState, stUpdate, app )
import DeclerationAST ( Decleration(..) )
import CommandAST
    ( UserFunction(GetInt, PrintInt),
      Command(BeginEnd, IfThenElse, Func, Assignment, WhileDo) )


-- temporary
import ProgramParser(parseProgram, Program(..))
import Parsers(parse)



type Identifier = String
type StackAddress = Int

type VarEnv = [(Identifier, StackAddress)]

type LName = String

-- MIGHT NEED OWN THING


-- data Program = LetIn [Decleration] [Command] deriving(Show)
-- data Command = Assignment Identifier AST | IfThenElse AST Command Command | WhileDo AST Command | Func UserFunction | BeginEnd [Command] deriving(Show)
-- data UserFunction = PrintInt AST | GetInt Identifier deriving(Show)

-- program
-- dedcompiler

doStuff = fst $ head $ parse parseProgram "let  var n; var x; var i in begin getint (n); if n < 0 then x := 0 else x := 1; i := 2; while i <= n do begin x := x * i; i := i + 1 end; printint (x) end "

quickTest = app (generateCode [("i", 3), ("x", 2), ("n", 1), ("reserved", 0)] (BeginEnd [Func (GetInt "n"),IfThenElse (BinOp Less (Variable "n") (LitInteger 0)) (Assignment "x" (LitInteger 0)) (Assignment "x" (LitInteger 1)),Assignment "i" (LitInteger 2),WhileDo (BinOp LessOrEq (Variable "i") (Variable "n")) (BeginEnd [Assignment "x" (BinOp Multiplication (Variable "x") (Variable "i")),Assignment "i" (BinOp Addition (Variable "i") (LitInteger 1))]),Func (PrintInt (Variable "x"))])) 0
-- testGen = app (generateCodeForProgram $ ) (([], 0), 0)

-- Add a variable to declarations called reserved, this is for the compiler to use when doing operations such <= >=, because they are made out of two operations
generateCodeForProgram (LetIn decl comm) =
    let (x, y) = app (dedCompiler $ (VarDecl "reserved"):decl) (([], 0), 2)
    in let varEnv = fst $ fst y
    in let labelIndex = snd y
    in let auu = app (generateCode varEnv (head comm)) (labelIndex)
    in (x ++ fst auu)

writeInstructions :: [TAMInst] -> IO()
writeInstructions s = writeFile "program.tam" (unlines (map show s)) 

-- Declerations
dedCompiler :: [Decleration] -> StateTransformer ((VarEnv, StackAddress), Int) [TAMInst]
dedCompiler (x:xs) = do
    a <- declTAM x
    b <- dedCompiler xs
    return (a ++ b)

dedCompiler [] = return []

declTAM (VarDecl v) = do
    ((ve, i), x) <- stState
    stUpdate (((v, i):ve, i+1), x)
    return [LOADL 0]

declTAM (VarDeclInit v e) = do
    ((ve, i), x) <- stState
    stUpdate (((v, i):ve, i+1), x)
    let an = app (expCode ve e) x
    ((ve', i'), x') <- stState
    stUpdate ((ve', i'), snd an)
    return $ (fst an)++[STORE i]
-- need to store the value of the expression in the variable



-- Commands
generateCode ve c = do
   code <- commCode ve c
   return $ code ++ [HALT]

commCode ve (IfThenElse e c1 c2) = do
   te <- expCode ve e
   l1 <- fresh
   l2 <- fresh
   tc1 <- commCode ve c1
   tc2 <- commCode ve c2
   return $ te ++ [JUMPIFZ l1] ++ tc1 ++ [JUMP l2, LABEL l1] ++ tc2 ++ [LABEL l2]

commCode ve (Func (PrintInt e)) = do
   te <- expCode ve e
   return $ te ++ [PUTINT]

commCode ve (Func (GetInt l)) = do
    return $ [GETINT] ++ [STORE $ findInVarEnv ve l]

commCode ve (Assignment i e) = do
    te <- expCode ve e
    return $ te ++ [STORE $ findInVarEnv ve i]

-- could be wrong, maybe do the command like the expression
commCode ve (WhileDo e c) = do
   te <- expCode ve e
   tc <- commCode ve c
   l1 <- fresh
   l2 <- fresh
   return $ [LABEL l1] ++ te ++ [JUMPIFZ l2] ++ tc ++ [JUMP l1] ++ [LABEL l2]

commCode ve (BeginEnd (x:xs)) = do
   tc <- commCode ve x
   rest <- (commCode ve (BeginEnd xs))
   return $ tc ++ rest

commCode ve (BeginEnd []) = return []

fresh :: StateTransformer Int LName
fresh = do
    a <- stState
    stUpdate (a+1)
    return ("#" ++ (show a))



-- Expressions
-- expCode :: VarEnv -> AST -> [TAMInst]
expCode ve ast = expressionTT ve ast

-- expressionT :: VarEnv -> AST -> [TAMInst]
-- expressionT ve (BinOp op t1 t2) = do
--     (expressionT ve t1) ++ (expressionT ve t2) ++ [getBinOperT op]

-- expressionTT :: [()] -> AST -> StateTransformer Int [TAMInst] 
expressionTT ve (BinOp op t1 t2) = do
    t1' <- expressionTT ve t1
    t2' <- expressionTT ve t2
    op <- getBinOperTT op
    return $ t1' ++ t2' ++ op

expressionTT ve (TernOp e1 e2 e3) = do
    t1 <- expressionTT ve e1
    l1 <- fresh
    t2 <- expressionTT ve e2
    l2 <- fresh
    t3 <- expressionTT ve e3
    return $ t1 ++ [JUMPIFZ l1] ++ t2 ++ [JUMP l2, LABEL l1] ++ t3 ++ [LABEL l2]

expressionTT ve (LitInteger a) = do
    return [LOADL (fromInteger a)]

expressionTT ve (UnOp op t1) = do
    t1' <- expressionTT ve t1
    op <- getUnOperTT op
    return $ t1' ++ op

expressionTT ve (Variable i) = do
    return [LOAD (findInVarEnv ve i)]

getBinOperTT Addition = return [ADD]
getBinOperTT Subtraction = return [SUB]
getBinOperTT Multiplication = return [MUL]
getBinOperTT Division = return [DIV]

getBinOperTT Less = return [LSS]
getBinOperTT More = return [GRT]
getBinOperTT Equiv = return [EQQ]

getBinOperTT And = return [AND]
getBinOperTT Or = return [OR]

-- might need to manually load in the values(because they are popped later on) back idk yet
getBinOperTT LessOrEq = return [LSS, STORE 0, EQQ, LOAD 0, OR]
getBinOperTT MoreOrEq = return [GRT, STORE 0, EQQ, LOAD 0, OR]

getBinOperTT NotEquiv = return [EQQ, NOT]

getUnOperTT Negation = return [NEG]
getUnOperTT ExclNegation = return [NOT]

-- expressionT ve (UnOp op t1) = (expressionT ve t1) ++ [getUnOperT op]
-- expressionT ve (Variable i) = [LOAD (findInVarEnv ve i)]

-- getBinOperT :: BinOperator -> TAMInst
-- getBinOperT Addition = ADD
-- getBinOperT Subtraction = SUB
-- getBinOperT Multiplication = MUL
-- getBinOperT Division = DIV

getUnOperT :: UnOperator -> TAMInst
getUnOperT Negation = NEG

-- General
findInVarEnv [] l = 0
findInVarEnv (x:xs) l = if (fst x) == l then snd x else findInVarEnv xs l