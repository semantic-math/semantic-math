module EditorTypesetter where

import Data.Maybe

import Data.Char (toCharCode)
import Data.Functor.Mu (roll, unroll)
import Data.Map (lookup)
import Editor (EditorNode)
import Editor as Editor
import Layout (LayoutNode)
import Layout as Layout
import Metrics (Metrics(..))
import Prelude (($), (+))

type Options = 
    { metrics :: Metrics
    , fontScale :: Number
    }

-- glyph :: LayoutNode
-- glyph = roll $ Layout.Glyph { char: 'a', size: 60.0 }

-- TODO: extract id from editorNode using getId
typeset :: Options -> EditorNode -> Maybe LayoutNode
typeset options editorNode = do
    let foo = (\x y -> x + y) :: Int -> Int -> Int

    -- let typeset' = (\editorNode -> )

    case unroll editorNode of
        Editor.Glyph {char} -> do
            let Metrics metrics = options.metrics
            case lookup char metrics.glyphMetrics of
                Just glyph -> Just $ roll $ Layout.Glyph { char, size: 60.0, metrics: glyph }
                Nothing -> Nothing
        Editor.Row {children} -> Nothing
        Editor.Sup {children} -> Nothing
        Editor.Frac {numerator, denominator} -> Nothing
        Editor.Parens {children} -> Nothing
