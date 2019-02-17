module Layout where
  
import Prelude

import Data.Functor.Mu (Mu)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

newtype Dist = Dist Number

instance showDist :: Show Dist where
  show (Dist d) = show d

type Dim = 
    { width :: Dist
    , ascent :: Dist
    , descent :: Dist
    }

data Kind = HBox | VBox

instance showKind :: Show Kind where
  show HBox = "Hbox"
  show VBox = "Vbox"

type Box = 
    { kind :: Kind
    , width :: Dist
    , ascent :: Dist
    , descent :: Dist
    , children :: Array LayoutNode
    }

data LayoutNodeF a
  = Box { dist :: Dist, box :: Box }
  | Glyph { char :: Char, size :: Number} -- , metrics :: Metrics }
  | Kern { dist :: Dist }
  | Rule { dim :: Dim }

type LayoutNode = Mu LayoutNodeF

derive instance functorLayoutNodeF :: Functor LayoutNodeF
derive instance genericLayoutNodeF :: Generic (LayoutNodeF a) _

instance showLayoutNodeF :: Show a => Show (LayoutNodeF a) where
  show = genericShow

