module Memory where

import Control.Monad.State.Lazy
import Data.Map (Map, empty, insert, findWithDefault)

type Value     = Int
type Address   = String
type Registers = Map Address Value
type Memory a  = State Registers a

-- =============================================================================

getAddr :: Address -> Memory Value
getAddr name = do
  rs <- get
  return $ findWithDefault 0 name rs

setAddr :: Address -> Value -> Memory Value
setAddr name val = do
  r <- getAddr name
  modify $ insert name (r + val)
  return r

checkValue :: Address -> (Value -> Bool) -> Memory Value
checkValue name predicate = do
  r <- getAddr name
  return $
    if predicate r
      then 1
      else 0
