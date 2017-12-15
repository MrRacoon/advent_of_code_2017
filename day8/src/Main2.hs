module Main where

import Prelude hiding (GT, LT, EQ)
import Data.Map (empty)
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Tok
import Control.Monad.State.Lazy

import Syntax
import Parser
import Memory

-- =============================================================================

toInt True  = 1
toInt False = 0

executeInstruction :: Expr -> Memory Value
executeInstruction inst =
  case inst of
    Register r   -> getAddr r
    Value v      -> return v

    BinOp EQ l r -> do
      ll <- executeInstruction l
      rr <- executeInstruction r
      return $ toInt $ ll == rr

    BinOp NE l r -> do
      ll <- executeInstruction l
      rr <- executeInstruction r
      return $ toInt $ ll /= rr

    BinOp GT l r -> do
      ll <- executeInstruction l
      rr <- executeInstruction r
      return $ toInt $ ll > rr

    BinOp GTE l r -> do
      ll <- executeInstruction l
      rr <- executeInstruction r
      return $ toInt $ ll >= rr

    BinOp LT l r -> do
      ll <- executeInstruction l
      rr <- executeInstruction r
      return $ toInt $ ll < rr

    BinOp LTE l r -> do
      ll <- executeInstruction l
      rr <- executeInstruction r
      return $ toInt $ ll <= rr

    BinOp Inc (Register name) r -> do
      ll <- getAddr name
      rr <- executeInstruction r
      let result = ll + rr
      setAddr name result
      return result

    BinOp Dec (Register name) r -> do
      ll <- getAddr name
      rr <- executeInstruction r
      let result = ll - rr
      setAddr name result
      return result

    BinOp If l r -> do
      rr <- executeInstruction r
      if rr == 1
        then executeInstruction l
        else return 0

execute :: [Expr] -> Memory Registers
execute []     = return empty
execute (x:xs) = do
  executeInstruction x
  execute xs
  get

-- =============================================================================

ast :: String -> [Expr]
ast str = case parseTopLevel str of
  Left err   -> []
  Right exps -> exps

executeExp exps = evalState (execute exps) empty
executeString = executeExp . ast

main :: IO ()
main = do
  test <- readFile "./testput.txt"
  contents <- readFile "./input.txt"
  let instructions = ast test
  print instructions
  print $ executeExp instructions
