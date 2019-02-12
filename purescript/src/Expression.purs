module Expression 
  ( numLit
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
  , Mu
  ) where
  
import Control.Apply (lift2)
import Data.Foldable (intercalate, foldl)
-- import Data.Functor.Mu (Mu, unroll, roll)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd, uncurry)
import Effect (Effect)
import Effect.Exception (throwException, error)
import Matryoshka.Class.Recursive (class Recursive)
import Matryoshka.Fold (para, cata)
import Prelude (class Functor, class Show, map, show, ($), (<>), (+), (*), (-), (/), pure, liftA1)
import Math as Math

newtype Mu f = In (f (Mu f))

roll :: forall f. f (Mu f) -> Mu f
roll = In

unroll :: forall f. Mu f -> f (Mu f)
unroll (In x) = x

instance recursiveMu :: Functor f => Recursive (Mu f) f where
  project = unroll

data NumericExprF a
  = NumLit    { id :: Int, value :: Number }
  | NumIdent  { id :: Int, name :: String }
  | Add       { id :: Int, args :: Array a }
  | Mul       { id :: Int, args :: Array a } -- TODO: differentiate implicit vs explicit multiplication
  | Sub       { id :: Int, minuend :: a, subtrahend :: a }
  | Div       { id :: Int, dividend :: a, divisor :: a }
  | Pow       { id :: Int, base :: a, exp :: a }
  | Abs       { id :: Int, arg :: a }
  | Fact      { id :: Int, arg :: a }
  | Root      { id :: Int, index :: Maybe a, radicand :: a }
  | Log       { id :: Int, base :: Maybe a, arg :: a }
  | Func      { id :: Int, name :: a, args :: Array a }
  | Sel       { id :: Int, indexes :: Array a}
--   | DotProd   { id :: Int, left :: VectorExprF a, right :: VectorExprF a }
  -- Sum
  -- Prod

type NumericExpr = Mu NumericExprF

derive instance functorNumericExprF :: Functor NumericExprF

numLit :: Number -> NumericExpr
numLit value = roll $ NumLit { id: 0, value }

add :: Array NumericExpr -> NumericExpr
add args = roll $ Add { id: 0, args }

mul :: Array NumericExpr -> NumericExpr
mul args = roll $ Mul { id: 0, args }

sub :: NumericExpr -> NumericExpr -> NumericExpr
sub minuend subtrahend = roll $ Sub { id: 0, minuend, subtrahend }

pow :: NumericExpr -> NumericExpr -> NumericExpr
pow base exp = roll $ Pow { id: 0, base, exp }

abs :: NumericExpr -> NumericExpr
abs arg = roll $ Abs { id: 0, arg }

fact :: NumericExpr -> NumericExpr
fact arg = roll $ Fact { id: 0, arg }

sqrt :: NumericExpr -> NumericExpr
sqrt radicand = roll $ Root { id: 0, index: Nothing, radicand }

root :: NumericExpr -> NumericExpr -> NumericExpr
root index radicand = roll $ Root { id: 0, index: Just index, radicand }

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

wrapInParens :: String -> String
wrapInParens a = "(" <> a <> ")"

showExpr :: NumericExprF (Tuple NumericExpr String) -> String
showExpr (NumLit {value}) = show value
showExpr (Add {args}) = intercalate " + " $ map snd args
showExpr (Mul {args}) = intercalate " * " $ 
  map (uncurry (\x y -> if isAdd x then wrapInParens y else y)) args
showExpr (Sub {minuend, subtrahend}) = (snd minuend) <> " - " <> (snd subtrahend)
showExpr (Div {dividend, divisor}) = (snd dividend) <> " - " <> (snd divisor)
showExpr (Pow {base, exp}) = do
  let exp' = uncurry (\x y -> if (isAdd x) then wrapInParens y else y) exp
  (snd base) <> "^" <> exp'
showExpr (Abs {arg: (Tuple _ arg)}) = "|" <> arg <> "|"
showExpr (Fact {arg: (Tuple _ arg)}) = arg <> "!"
showExpr (Root {index, radicand}) = do
  let index' = snd $ fromMaybe (Tuple (numLit 2.0) "2") index
  "root(" <> index' <> ", " <> (snd radicand) <> ")"
showExpr (Sel {indexes}) = "_" <> intercalate "," (map snd indexes)
-- TODO: DotProd
showExpr _ = ""

instance showNumericExpr :: Show (Mu NumericExprF) where
  show = para showExpr

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
