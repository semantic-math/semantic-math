module Main where

import Expression

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, ($), show, bind)


main :: Effect Unit
main = do
  log "Hello world!"
  let expr = mul [numLit 4.0, add [numLit 2.0, numLit 3.0]] :: NumericExpr
  log $ show $ unroll expr
  log $ showExpr' expr
  result <- eval expr
  log $ show result
