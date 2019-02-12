module Main where

import Prelude (Unit, discard, ($), show, bind)
import Effect (Effect)
import Effect.Console (log)
import Expression

main :: Effect Unit
main = do
  log "Hello world!"
  let expr = mul [numLit 4.0, add [numLit 2.0, numLit 3.0]]
  log $ show expr
  result <- eval expr
  log $ show result
