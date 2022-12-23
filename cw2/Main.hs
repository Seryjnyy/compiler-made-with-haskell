module Main where
import System.Environment ( getArgs ) 
import Parsers(parse)
import ProgramParser(parseProgram)
import TAMVirtualMachine(executeProgram, TAMInst(..))
import CodeGenerator(generateCodeForProgram, writeInstructions)

main :: IO ()
main = do
    args <- getArgs
    case readFileExtension (head args) of
        "mt" -> do 
            file <- readFile (head args)
            let code = concat (map (++" ") $ lines file)
            let asts = parse parseProgram code
            let inst = generateCodeForProgram $ fst $ head $ asts
            writeInstructions inst
            return ()
        "tam" -> do
            file <- readFile (head args)
            let v  = map (read) (lines file) :: [TAMInst]
            executeProgram v
            return ()

readFileExtension :: String -> String
readFileExtension s = reverse(takeWhile (/='.') (reverse s))