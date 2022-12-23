module TAMVirtualMachine(TAMInst(..), executeProgram) where

import StateTransformer(StateIO(..), appIO, lift, stStateIO, stUpdateIO)

type LName = String
type Identifier = String
type StackAddress = Int

type MTInt = Int
type Stack = [MTInt]                                     -- new commands
data TAMInst = LOADL MTInt | MUL | DIV | ADD | SUB | NEG 
 | LOAD StackAddress | STORE StackAddress | LABEL LName | JUMP LName 
 | JUMPIFZ LName | HALT | AND | OR | NOT | LSS | GRT | EQQ | PUTINT | GETINT deriving(Show, Eq, Read)

data TAMState = TAMState {
   tsCode :: [TAMInst],
   tsCounter :: Int,
   tsStack :: Stack
} deriving(Show)


type TAMStIO a = StateIO TAMState a


-- SETUP FOR EXECUTION
-- currently not using
newTAMState n cs stk= TAMState{tsCounter=n, tsCode=cs, tsStack=stk}


executeProgram code = do
     appIO (executeAll) TAMState{tsCounter = 0, tsStack =[], tsCode = code}


test = do
    x <- appIO (executeAll) TAMState{tsCounter=0, tsStack=[], tsCode=[LOADL 0,LOADL 0,LOADL 0,LOADL 0,GETINT,STORE 1,LOAD 1,LOADL 0,LSS,JUMPIFZ "#2",LOADL 0,STORE 2,JUMP "#3",LABEL "#2",LOADL 1,STORE 2,LABEL "#3",LOADL 2,STORE 3,LABEL "#4",LOAD 3,LOAD 1,LSS,STORE 0,EQQ,LOAD 0,OR,JUMPIFZ "#5",LOAD 2,LOAD 3,MUL,STORE 2,LOAD 3,LOADL 1,ADD,STORE 3,JUMP "#4",LABEL "#5",LOAD 2,PUTINT,HALT]}
    return x
-- START EXECUTE FUNCTIONS
executeAll = do
    i <- counterT
    cs <- codeT
    if i /= (-1) then 
        do 
            (executeT (cs!!i)) 
            executeAll  
        else return ()

executeT (LOADL a) = do
    pushT a
    continueT

executeT (LOAD a) = do
    x <- getFromT a
    pushT x
    continueT

executeT (STORE a) = do
    x <- popT
    insertAtT a x
    continueT

executeT (LABEL l) = do
    continueT

executeT (JUMP l) = do
    jumpToLabelT l

executeT (JUMPIFZ l) = do
    x <- popT
    if x == 0 then jumpToLabelT l else continueT

-- might be different depending on the function that executes everything
executeT HALT = do
    setCounterT (-1)

executeT ADD = do
    x <- popT
    y <- popT
    pushT (y + x)
    continueT

executeT SUB = do
    x <- popT
    y <- popT
    pushT (y - x)
    continueT

executeT MUL = do
    x <- popT
    y <- popT
    pushT (y * x)
    continueT

executeT DIV = do
    x <- popT
    y <- popT
    pushT (y `div` x)
    continueT

executeT NEG = do
    x <- popT
    pushT (-x)
    continueT

-- these inst might not consume the popped values, might need to put them back with result at top
executeT AND = do
    x <- popT
    y <- popT
    pushT x
    pushT y
    pushT (boolIntoIntT (intIntoBoolT x && intIntoBoolT y))
    continueT

executeT OR = do
    x <- popT
    y <- popT
    pushT x
    pushT y
    pushT (boolIntoIntT (intIntoBoolT x || intIntoBoolT y))
    continueT

executeT NOT = do
    x <- popT
    pushT x
    pushT (boolIntoIntT $ not (intIntoBoolT x))
    continueT

executeT LSS = do
    x <- popT
    y <- popT
    pushT x
    pushT y
    pushT (boolIntoIntT(y<x))
    continueT

executeT GRT = do
    x <- popT
    y <- popT
    pushT x
    pushT y
    pushT (boolIntoIntT(y>x))
    continueT

executeT EQQ = do
    x <- popT
    y <- popT
    pushT x
    pushT y
    pushT (boolIntoIntT(y==x))
    continueT

executeT PUTINT = do
    x <- popT
    lift $ putStrLn("Output: " ++ (show x)) 
    continueT

executeT GETINT = do
  lift $ putStrLn "Enter a number: "
  s <- lift getLine
  pushT (read s)
  continueT

-- END EXECUTE FUNCTIONS

-- START AUXILLARY EXECUTE FUNCTIONS

-- find the label in the code, and return the index, use that to set the counter

intIntoBoolT n = not (n == 0)

boolIntoIntT b = if b then 1 else 0

jumpToLabelT l = do
    i <- findLabelIndexT (LABEL l)
    setCounterT i

findLabelIndexT l = do
    cs <- codeT
    return (findInList cs l 0)

findInList [] l i = i
findInList (x:xs) l i = if x == l then i else findInList xs l (i+1)

codeT = do 
    ts <- stStateIO
    return (tsCode ts)

tsIncrement = do
    ts <- stStateIO
    TAMState{tsCounter} <- stStateIO
    stUpdateIO (ts{tsCounter= tsCounter + 1})

popT = do 
    stk <- stackT
    stkUpdateT (tail stk) -- tail returns list without head
    return (head stk)

getFromT i = do
    stk <- stackT
    return $ stk !! ((length stk) - 1 - i)

insertAtT i n = do
    stk <- stackT
    stkUpdateT((take ((length stk) - 1 - i) stk) ++ [n] ++ (drop ((length stk) - i) stk))

pushT n = do
    stk <- stackT
    stkUpdateT (n:stk)
    return ()

stackT :: TAMStIO Stack
stackT = do 
    ts <- stStateIO
    return (tsStack ts)

continueT = do 
    ts <- stStateIO
    TAMState{tsCounter} <- stStateIO
    stUpdateIO (ts{tsCounter = tsCounter+1})

setCounterT n = do
    ts <- stStateIO
    TAMState{tsCounter} <- stStateIO
    stUpdateIO (ts{tsCounter = n})

counterT = do
    ts <- stStateIO
    return (tsCounter ts)

stkUpdateT :: Stack -> TAMStIO ()
stkUpdateT stk = do 
    ts <- stStateIO
    stUpdateIO (ts{tsStack = stk})


-- END AUXILLARY EXECUTE FUNCTIONS