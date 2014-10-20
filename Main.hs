-- automatically generated by BNF Converter
module Main where


import System.IO ( stdin, stderr, hGetContents, hPutStrLn )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexVar
import ParVar
import PrintVar
import AbsVar
import qualified StInterpreter as St
import Interpreter


import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrLn s
                          exitFailure
           Ok  tree -> do --showTree v tree
                          lr <- St.runInterpretation tree
                          case lr of
                            (Left msg) -> hPutStrLn stderr ("STATIC ANALYSIS: " ++ msg)
                            (Right _) -> runInterpretation tree

                          exitSuccess


showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> hGetContents stdin >>= run 2 pProgram
            "-s":fs -> mapM_ (runFile 0 pProgram) fs
            fs -> mapM_ (runFile 2 pProgram) fs




