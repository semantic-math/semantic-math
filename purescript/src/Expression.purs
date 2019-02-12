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
  , Mu
  , roll
  , unroll
  , showExpr'
  , getId
  ) where
  
import Data.Generic.Rep

import Control.Apply (lift2)
import Data.Foldable (intercalate, foldl)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.TacitString as TS
import Data.Tuple (Tuple(..), snd, uncurry)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Math as Math
import Matryoshka.Class.Recursive (class Recursive)
import Matryoshka.Fold (para, cata)
import Prelude (class Functor, class Show, bind, discard, liftA1, map, pure, show, ($), (*), (+), (-), (/), (<#>), (<>), (>>>))


foreign import getId :: forall a. NumericExprF a -> Int

newtype Mu f = In (f (Mu f))

roll :: forall f. f (Mu f) -> Mu f
roll = In

unroll :: forall f. Mu f -> f (Mu f)
unroll (In x) = x

instance recursiveMu :: Functor f => Recursive (Mu f) f where
  project = unroll

instance showMu :: (Show (f TS.TacitString), Functor f) => Show (Mu f) where
  show (In x) = show $ x <#> (show >>> TS.hush)

-- derive instance genericMu :: Generic (Mu a) _

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
derive instance genericMuNumericExprF :: Generic (Mu NumericExprF) _
derive instance genericNumericExprF :: Generic (NumericExprF a) _

instance showNumericExprF :: Show a => Show (NumericExprF a) where
  show = genericShow

-- findInSum :: forall a b c. Constructor _ a => Sum a b -> c
-- findInSum sum = case sum of
--   Inl a -> a
--   Inr b -> b

-- define a type class for getting the id from a Sum
-- class GShow a where
--   gShow :: a -> String

-- Now provide instances for GShow for the appropriate representation types.
-- Note: we don't have to implement all instances here.

-- instance gShowU1 :: GShow U1 where
--   gShow _ = ""

-- instance gShowSum :: (GShow a, GShow b) => GShow (a + b) where
--   gShow (Inl a) = gShow a
--   gShow (Inr b) = gShow b

-- getId' :: forall t45 t48 t51.
--   Sum
--     (Constructor t48
--        (Argument
--           { id :: Int
--           | t51
--           }
--        )
--     )
--     t45
--   -> Int
-- getId' x = do
--   case x of  
--     Inl (Constructor (Argument a)) -> a.id
--     Inr c -> 5

-- getId :: NumericExpr -> Int
-- getId expr = do
--   let expr' = from $ unroll expr
--   case expr' of
--     Inl (Constructor (Argument a)) -> a.id
--     Inr b -> do
--       case b of
--         Inl (Constructor (Argument a)) -> a.id
--         Inr c -> do
--           case c of
--             Inl (Constructor (Argument a)) -> a.id
--             Inr _ -> 10

-- getId' :: NumericExpr -> Int
-- getId' expr = do
--   case unroll expr of
--     NumLit {id} -> id
--     NumIdent {id} -> id
--     Add {id} -> id
--     Mul {id} -> id
--     _ -> -1

idRef :: Ref.Ref Int
idRef = unsafePerformEffect (Ref.new 0)

genId :: Effect Int
genId = do
  result <- Ref.read idRef
  Ref.write (result + 1) idRef
  pure result

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

showExpr :: NumericExprF (Tuple NumericExpr String) -> String
showExpr (NumLit {id, value}) = show value
showExpr (Add {args}) = intercalate " + " $ map snd args
showExpr (Mul {args}) = intercalate " * " $ 
  map (uncurry (\x y -> if isAdd x then "(" <> y <> ")" else y)) args
showExpr (Sub {minuend, subtrahend}) = (snd minuend) <> " - " <> (snd subtrahend)
showExpr (Div {dividend, divisor}) = (snd dividend) <> " - " <> (snd divisor)
showExpr (Pow {base, exp}) = do
  let exp' = uncurry (\x y -> if (isAdd x) then "(" <> y <> ")" else y) exp
  (snd base) <> "^" <> exp'
showExpr (Abs {arg: (Tuple _ arg)}) = "|" <> arg <> "|"
showExpr (Fact {arg: (Tuple _ arg)}) = arg <> "!"
showExpr (Root {index, radicand}) = do
  let index' = snd $ fromMaybe (Tuple (numLit 2.0) "2") index
  "root(" <> index' <> ", " <> (snd radicand) <> ")"
showExpr (Sel {indexes}) = "_" <> intercalate "," (map snd indexes)
-- TODO: DotProd
showExpr _ = ""

showExpr' :: NumericExpr -> String
showExpr' = para showExpr

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
