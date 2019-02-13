module Expression 
  ( numLit
  , numIdent
  , add
  , mul
  , sub
  , pow
  , abs
  , fact
  , sqrt
  , root
  , eval
  , NumericExprF
  , NumericExpr
  , showExpr
  , getId
  ) where
  
import Data.Generic.Rep

import Control.Apply (lift2)
import Data.Foldable (intercalate, foldl)
import Data.Functor.Mu (Mu, roll, unroll)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd, uncurry)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Unsafe (unsafePerformEffect)
import Math as Math
import Matryoshka.Fold (para, cata)
import Prelude (class Functor, class Show, liftA1, map, pure, show, ($), (*), (+), (-), (/), (<>))
import UniqueId (genId)

foreign import getId :: NumericExpr -> Int

type Common = ( id :: Int )

data NumericExprF a
  = NumLit    { value :: Number | Common }
  | NumIdent  { name :: String | Common }
  | Add       { args :: Array a | Common }
  | Mul       { args :: Array a | Common } -- TODO: differentiate implicit vs explicit multiplication
  | Sub       { minuend :: a, subtrahend :: a | Common }
  | Div       { dividend :: a, divisor :: a | Common }
  | Pow       { base :: a, exp :: a | Common }
  | Abs       { arg :: a | Common }
  | Fact      { arg :: a | Common }
  | Root      { index :: Maybe a, radicand :: a | Common }
  | Log       { base :: Maybe a, arg :: a | Common }
  | Func      { name :: a, args :: Array a | Common }
  | Sel       { indexes :: Array a | Common }
--   | DotProd   { id :: Int, left :: VectorExprF a, right :: VectorExprF a }
  -- Sum
  -- Prod

type NumericExpr = Mu NumericExprF

derive instance functorNumericExprF :: Functor NumericExprF
derive instance genericNumericExprF :: Generic (NumericExprF a) _

instance showNumericExprF :: Show a => Show (NumericExprF a) where
  show = genericShow


numLit :: Number -> NumericExpr
numLit value = roll $ NumLit { id: unsafePerformEffect genId, value }

numIdent :: String -> NumericExpr
numIdent name = roll $ NumIdent { id: unsafePerformEffect genId, name }

add :: Array NumericExpr -> NumericExpr
add args = roll $ Add { id: unsafePerformEffect genId, args }

mul :: Array NumericExpr -> NumericExpr
mul args = roll $ Mul { id: unsafePerformEffect genId, args }

sub :: NumericExpr -> NumericExpr -> NumericExpr
sub minuend subtrahend = roll $ Sub { id: unsafePerformEffect genId, minuend, subtrahend }

pow :: NumericExpr -> NumericExpr -> NumericExpr
pow base exp = roll $ Pow { id: unsafePerformEffect genId, base, exp }

abs :: NumericExpr -> NumericExpr
abs arg = roll $ Abs { id: unsafePerformEffect genId, arg }

fact :: NumericExpr -> NumericExpr
fact arg = roll $ Fact { id: unsafePerformEffect genId, arg }

sqrt :: NumericExpr -> NumericExpr
sqrt radicand = roll $ Root { id: unsafePerformEffect genId, index: Nothing, radicand }

root :: NumericExpr -> NumericExpr -> NumericExpr
root index radicand = roll $ Root { id: unsafePerformEffect genId, index: Just index, radicand }

-- TODO: switch to operator precedence
isAdd :: NumericExpr -> Boolean
isAdd x = case unroll x of
  Add {} -> true
  _ -> false

-- TODO: switch to operator precedence
isMul :: NumericExpr -> Boolean
isMul x = case unroll x of
  Mul {} -> true
  _ -> false

_showExpr :: NumericExprF (Tuple NumericExpr String) -> String
_showExpr (NumLit {id, value}) = show value
_showExpr (Add {args}) = intercalate " + " $ map snd args
_showExpr (Mul {args}) = intercalate " * " $ 
  map (uncurry (\x y -> if isAdd x then "(" <> y <> ")" else y)) args
_showExpr (Sub {minuend, subtrahend}) = (snd minuend) <> " - " <> (snd subtrahend)
_showExpr (Div {dividend, divisor}) = (snd dividend) <> " - " <> (snd divisor)
_showExpr (Pow {base, exp}) = do
  let exp' = uncurry (\x y -> if (isAdd x) then "(" <> y <> ")" else y) exp
  (snd base) <> "^" <> exp'
_showExpr (Abs {arg: (Tuple _ arg)}) = "|" <> arg <> "|"
_showExpr (Fact {arg: (Tuple _ arg)}) = arg <> "!"
_showExpr (Root {index, radicand}) = do
  let index' = snd $ fromMaybe (Tuple (numLit 2.0) "2") index
  "root(" <> index' <> ", " <> (snd radicand) <> ")"
_showExpr (Sel {indexes}) = "_" <> intercalate "," (map snd indexes)
-- TODO: DotProd
_showExpr _ = ""

showExpr :: NumericExpr -> String
showExpr = para _showExpr

evalExpr :: NumericExprF (Effect Number) -> Effect Number
evalExpr (NumIdent {name}) = throwException $ error $ "variable '" <> name <> "' is undefined"
evalExpr (NumLit {value}) = pure value
evalExpr (Add {args}) = foldl (lift2 (+)) (pure 0.0) args
evalExpr (Mul {args}) = foldl (lift2 (*)) (pure 1.0) args
evalExpr (Sub {minuend, subtrahend}) = lift2 (-) minuend subtrahend
evalExpr (Div {dividend, divisor}) = lift2 (/) dividend divisor
evalExpr (Pow {base, exp}) = lift2 Math.pow base exp
evalExpr (Abs {arg}) = liftA1 Math.abs arg
-- evalExpr (Fact {arg}) = liftA1 Math.abs arg
evalExpr (Root {index, radicand}) = do
  let index' = fromMaybe (pure 2.0) index
  lift2 Math.pow radicand (lift2 (/) (pure 1.0) index')
evalExpr (Func {name}) = throwException $ error $ "we don't handle functions yet"
evalExpr (Sel {indexes}) = do
  case indexes of
    [] -> throwException $ error $ "empty selector (no indexes)"
    _  -> throwException $ error $ "we don't handle selectors yet"
evalExpr (Log {base, arg}) = do
  let base' = fromMaybe (pure Math.e) base
  lift2 (/) (liftA1 Math.log arg) (liftA1 Math.log base')
-- evalExpr (DotProd {left, right}) = throwException $ error $ "we don't handle vectors yet"
evalExpr _ = throwException $ error "Unhandled operation"

eval :: NumericExpr -> Effect Number
eval = cata evalExpr
