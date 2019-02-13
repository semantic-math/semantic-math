module UniqueId (genId) where

import Prelude (bind, discard, (+), pure)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

idRef :: Ref.Ref Int
idRef = unsafePerformEffect (Ref.new 0)

-- TODO: provide a way to create multiple generators

genId :: Effect Int
genId = do
  result <- Ref.read idRef
  Ref.write (result + 1) idRef
  pure result
