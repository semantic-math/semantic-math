module EditorTypesetter where

import Prelude (Unit, ($))
import Editor (EditorNode)
import Layout (LayoutNode, LayoutNodeF(..))
import Data.Functor.Mu (roll)

newtype Metrics = Metrics Unit

type Options = 
    { metrics :: Metrics
    , fontScale :: Number
    }

glyph :: forall t5. LayoutNodeF t5
glyph = Glyph { char: 'a', size: 60.0 }

-- TODO: extract id from editorNode using getId
typeset :: Options -> EditorNode -> LayoutNode
typeset options editorNode = roll $ Glyph { char: 'a', size: 60.0 }
