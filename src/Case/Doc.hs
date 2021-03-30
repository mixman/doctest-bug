{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Case.Doc (
    Login
    ) where
import Control.Applicative (Alternative (..), optional)
import GHC.Generics                (Generic)
import qualified Data.Text                            as T
import qualified Kleene.Functor                       as K
import Language.Haskell.TH.Lift    (deriveLift)
import Data.Text                   (Text)

-- | Login name. @[a-z]{4,5}|itteam@.
newtype Login = Login Text
  deriving (Eq, Ord, Generic)

deriveLift ''Login

-- | Login's regular expression.
--
-- >>> K.putPretty loginKleene
-- ^[a-z][a-z][a-z][a-z][a-z]?$
--
loginKleene :: K.K Char Login
loginKleene = Login . T.pack <$> range 4 5 (K.charRange 'a' 'z')
  where
    range
        :: Alternative f
        => Int  -- ^ min
        -> Int  -- ^ max
        -> f a
        -> f [a]
    range mi ma f = go mi ma
      where
        go start end
            | start > end || end <= 0 = pure []
            | start > 0 = (:) <$> f <*> go (start - 1) (end - 1)
            | otherwise = inRange <$> optional f <*> go 0 (end - 1)

        inRange current next = maybe [] (:next) current

-- $setup
-- >>> import qualified Kleene.Internal.Pretty as K
