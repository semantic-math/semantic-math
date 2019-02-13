module Main where

import Data.Functor.Mu (unroll)
import Editor (glyph, row)
import Effect (Effect)
import Effect.Console (log)
import Expression (NumericExpr, add, eval, getId, mul, numIdent, numLit, showExpr)
import Prelude (Unit, discard, ($), (<>), show, bind)

main :: Effect Unit
main = do
  log "Hello world!"
  let expr = mul [numLit 4.0, add [numLit 2.0, numLit 3.0]] :: NumericExpr
  log $ show $ unroll expr
  log $ showExpr expr
  result <- eval expr
  log $ show result
  log $ "foo's id = " <> show (getId $ numIdent "foo")
  log $ "1.23's id = " <> show (getId $ numLit 1.23)

  let tree = row [glyph 'x']
  log $ "edit tree = " <> show tree
