module Main where

import Text.ParserCombinators.Parsec
import Data.List ((\\))

-- =============================================================================

type Name = String
type Weight = Int

data Tree
  = Leaf Name Weight
  | Branch Name Weight [Name]
  deriving (Show)

-- =============================================================================

parseFile :: String -> Either ParseError [Tree]
parseFile = parse file ""

file :: GenParser Char st [Tree]
file = do
  ts <- many1 tree
  eof
  return ts

-- =============================================================================

tree :: GenParser Char st Tree
tree = try leaf <|> branch <?> "Could not parse treeType"

-- =============================================================================

nameAndWeight = do
  n <- name
  space
  w <- weight
  return (n, w)

leaf :: GenParser Char st Tree
leaf = do
  (n,w) <- nameAndWeight
  eol
  return $ Leaf n w

branch :: GenParser Char st Tree
branch = do
  (n,w) <- nameAndWeight
  space
  arrow
  space
  cs <- children
  eol
  return $ Branch n w cs

-- =============================================================================

name :: GenParser Char st Name
name = many1 letter

weight :: GenParser Char st Weight
weight = do
  openParen
  w <- many1 digit
  closeParen
  return $ read w

children :: GenParser Char st [Name]
children = many $ try notLastChild <|> lastChild

notLastChild :: GenParser Char st Name
notLastChild = do
  n <- name
  comma
  space
  return n

lastChild :: GenParser Char st Name
lastChild = name

-- =============================================================================

openParen :: GenParser Char st String
openParen = string "("

closeParen :: GenParser Char st String
closeParen = string ")"

arrow :: GenParser Char st String
arrow = string "->"

comma :: GenParser Char st String
comma = string ","

eol :: GenParser Char st String
eol = string "\n"

-- =============================================================================

getName :: Tree -> Name
getName (Leaf n _) = n
getName (Branch n _ _) = n

getAllNames :: [Tree] -> [Name]
getAllNames = map getName

getChildren :: Tree -> [Name]
getChildren (Leaf _ _) = []
getChildren (Branch _ _ cs) = cs

getAllChildren :: [Tree] -> [Name]
getAllChildren = concatMap getChildren

-- =============================================================================

main :: IO ()
main = do
  contents <- readFile "./src/input.txt"
  case parseFile contents of
    Left err -> do
      putStrLn "could not parse thing"
      print err
    Right ts -> do
      let ns = getAllNames ts
          cs = getAllChildren ts
          diff = ns \\ cs
      print diff
