module Metrics where

import Data.Generic.Rep
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.:))
import Prelude (class Show, bind, pure, ($))
import Data.Generic.Rep.Show (genericShow)
import Foreign.Object (Object)

data Glyph = Glyph
    { advance :: Number
    , bearingX :: Number
    , bearingY :: Number
    , height :: Number
    , width :: Number
    }

derive instance genericGlyph :: Generic Glyph _
instance showGlyph :: Show Glyph where
  show x = genericShow x

newtype Metrics = Metrics
    { unitsPerEm :: Number
    , glyphMetrics :: Object Glyph
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
        glyphMetrics <- x .: "glyphMetrics" -- TODO convert this to Map Int Glyph
        pure $ Metrics { unitsPerEm, glyphMetrics }
