module Main where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, bind, discard, show, void, ($), (<>))
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..), fromRight)
import Data.HTTP.Method (Method(..))
import Data.Argonaut.Decode.Class (decodeJson)
import Partial.Unsafe (unsafePartial)
import Metrics (Metrics(..))
import Expression (NumericExpr, add, eval, getId, mul, numIdent, numLit, showExpr)
import Editor (glyph, row)
import Data.Functor.Mu (unroll)
-- import Debug.Trace

main :: Effect Unit
main = void $ launchAff $ do
    liftEffect $ log "fetching comic-sans.json"
    res <- AX.request (
        AX.defaultRequest { 
            url = "/comic-sans.json", 
            method = Left GET, 
            responseFormat = ResponseFormat.json 
        }
    )
    liftEffect do
        log "finished feteching"
        let body = unsafePartial $ fromRight res.body
        let Metrics foo = unsafePartial $ fromRight $ decodeJson body
        log $ show foo.unitsPerEm
        log $ show foo.glyphMetrics
        -- let _ = spy "Hello" foo.glyphMetrics
        log "finished"
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
