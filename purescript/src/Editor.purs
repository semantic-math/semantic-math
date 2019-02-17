module Editor where

import Prelude

import Data.Functor.Mu (Mu, roll)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect.Unsafe (unsafePerformEffect)
import UniqueId (genId)

type Common = ( id :: Int )

data EditorNodeF a
  = Glyph { char :: Char | Common }
  | Row { children :: Array a | Common }
  | Sup { children :: Array a | Common }
  | Frac { numerator :: Array a, denominator :: Array a | Common }
  | Parens { children :: Array a | Common }

derive instance functorEditorNodeF :: Functor EditorNodeF
derive instance genericEditorNodeF :: Generic (EditorNodeF a) _

instance showEditorNodeF :: Show a => Show (EditorNodeF a) where
  show = genericShow

type EditorNode = Mu EditorNodeF

glyph :: Char -> EditorNode
glyph char = roll $ Glyph { char, id: unsafePerformEffect genId }

row :: Array EditorNode -> EditorNode
row children = roll $ Row { children, id: unsafePerformEffect genId }

sup :: Array EditorNode -> EditorNode
sup children = roll $ Sup { children, id: unsafePerformEffect genId }

frac :: Array EditorNode -> Array EditorNode -> EditorNode
frac numerator denominator = roll $ Frac { numerator, denominator, id: unsafePerformEffect genId }

parens :: Array EditorNode -> EditorNode
parens children = roll $ Parens { children, id: unsafePerformEffect genId }




