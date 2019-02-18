module Layout where
  
import Prelude

import Data.Foldable (sum, foldl)
import Data.Functor.Mu (Mu(..), roll)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (unwrap, class Newtype)
import Metrics as Metrics

newtype Dist = Dist Number
derive instance newtypeDist :: Newtype Dist _

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

type BoxF a = 
    { kind :: Kind
    , width :: Dist
    , ascent :: Dist
    , descent :: Dist
    , children :: Array a
    }

type Common = ( id :: Int )

data LayoutNodeF a
    = Box { dist :: Number, box :: BoxF a | Common }
    | Glyph { char :: Char, size :: Number, metrics :: Metrics.Glyph | Common }
    | Kern { dist :: Dist | Common }
    | Rule { dim :: Dim | Common }

type LayoutNode = Mu LayoutNodeF

derive instance functorLayoutNodeF :: Functor LayoutNodeF
derive instance genericLayoutNodeF :: Generic (LayoutNodeF a) _

instance showLayoutNodeF :: Show a => Show (LayoutNodeF a) where
    show = genericShow

width :: LayoutNode -> Number
width (In (Box {box})) = unwrap box.width
width (In (Glyph {size, metrics})) = size * (unwrap metrics).advance
width (In (Kern {dist})) = unwrap dist
width (In (Rule {dim})) = unwrap dim.width

ascent :: LayoutNode -> Number
ascent (In (Box {box})) = unwrap box.ascent
ascent (In (Glyph {size, metrics})) = size * (unwrap metrics).bearingY
ascent (In (Kern {})) = 0.0
ascent (In (Rule {dim})) = unwrap dim.ascent

descent :: LayoutNode -> Number
descent (In (Box {box})) = unwrap box.descent
descent (In (Glyph {size, metrics})) = size * ((unwrap metrics).height - (unwrap metrics).bearingY)
descent (In (Kern {})) = 0.0
descent (In (Rule {dim})) = unwrap dim.descent

vwidth :: LayoutNode -> Number
vwidth (In (Box {box})) = unwrap box.width
vwidth (In (Glyph {size, metrics})) = size * (unwrap metrics).advance
vwidth (In (Kern {dist})) = 0.0
vwidth (In (Rule {dim})) = unwrap dim.width

vheight :: LayoutNode -> Number
vheight (In (Box {box})) = (unwrap box.ascent) + (unwrap box.descent)
vheight (In (Glyph {size, metrics})) = size * (unwrap metrics).height
vheight (In (Kern {dist})) = unwrap dist
vheight (In (Rule {dim})) = (unwrap dim.ascent) + (unwrap dim.descent)

hlistWidth :: Array LayoutNode -> Number
hlistWidth = sum <<< map width

max' :: forall a. Ord a => Semiring a => Array a -> a
max' xs = foldl max zero xs

hlistAscent :: Array LayoutNode -> Number
hlistAscent = max' <<< map ascent

hlistDescent :: Array LayoutNode -> Number
hlistDescent = max' <<< map descent

vlistWidth :: Array LayoutNode -> Number
vlistWidth = max' <<< map vwidth

vlistsHeight :: Array LayoutNode -> Number
vlistsHeight = sum <<< map vheight

hpackNat :: Array LayoutNode -> LayoutNode
hpackNat children = roll $ Box { 
    id: 0,
    dist: 0.0, 
    box: {
        kind: HBox,
        width: Dist $ hlistWidth children,
        ascent: Dist $ hlistAscent children,
        descent: Dist $ hlistDescent children,
        children
    }
}
