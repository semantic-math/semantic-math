module Metrics where

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Char (fromCharCode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.Map (Map, empty, insert)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype)
import Foreign.Object (fold)
import Partial.Unsafe (unsafePartial)
import Prelude (class Show, bind, pure, ($))

newtype Glyph = Glyph
    { advance :: Number
    , bearingX :: Number
    , bearingY :: Number
    , height :: Number
    , width :: Number
    }

derive instance genericGlyph :: Generic Glyph _
derive instance newtypeGlyph :: Newtype Glyph _
instance showGlyph :: Show Glyph where
  show x = genericShow x

newtype Metrics = Metrics
    { unitsPerEm :: Number
    , glyphMetrics :: Map Char Glyph
    }

instance decodeJsonGlyph :: DecodeJson Glyph where
    decodeJson json = do
        x <- decodeJson json
        advance <- x .: "advance"
        bearingX <- x .: "bearingX"
        bearingY <- x .: "bearingY"
        height <- x .: "height"
        width <- x .: "width"
        pure $ Glyph { advance, bearingX, bearingY, height, width }


instance decodeJsonMetrics :: DecodeJson Metrics where
    decodeJson json = do
        x <- decodeJson json
        unitsPerEm <- x .: "unitsPerEm"
        glyphMetricsObject <- x .: "glyphMetrics"
        -- TODO: update comic-sans.json to use characters as keys
        let glyphMetrics = fold (\accu key glyph -> 
            insert (unsafePartial $ fromJust $ fromCharCode $ unsafePartial $ fromJust $ fromString key) glyph accu) empty glyphMetricsObject
        pure $ Metrics { unitsPerEm, glyphMetrics }
