{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE Trustworthy         #-}
module Case.Colour (
    colourRGB8,
    ) where
import Codec.Picture.Types (PixelCMYK8 (..), PixelRGB8 (..))
import Control.DeepSeq             (NFData (..))
import qualified Data.Colour.SRGB as DC
import Data.Hashable               (Hashable (..))
import Data.Typeable               (Typeable)
import GHC.Generics                (Generic)

data AccentFamily
    = AF1  -- ^ Greens
    | AF2  -- ^ Violets
    | AF3  -- ^ Blues
    | AF4  -- ^ Yellow, orange, red
    | AF5  -- ^ Light colds
    | AF6  -- ^ Light colourfuls
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, Typeable)

instance NFData AccentFamily
instance Hashable AccentFamily

data AccentColour
    = AC1 | AC2 | AC3
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, Typeable)

instance NFData AccentColour
instance Hashable AccentColour

data Colour
    = FutuGreen       -- ^ Futurice Green
    | FutuBlack       -- ^ Black
    | FutuLightGreen  -- ^ Accent Green Light, not in palette anymore
    | FutuDarkGreen   -- ^ Accent Green Dark, not in palette anymore
    | FutuAccent AccentFamily AccentColour
      -- ^ Accent Colours
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Enum Colour where
    toEnum 0 = FutuGreen
    toEnum 1 = FutuBlack
    toEnum 2 = FutuLightGreen
    toEnum 3 = FutuDarkGreen
    toEnum n = FutuAccent (toEnum f) (toEnum c)
      where (f, c) = quotRem (n - 4) 3

    fromEnum FutuGreen        = 0
    fromEnum FutuBlack        = 1
    fromEnum FutuLightGreen   = 2
    fromEnum FutuDarkGreen    = 3
    fromEnum (FutuAccent f c) = 4 + 3 * fromEnum f + fromEnum c

instance Bounded Colour where
    minBound = FutuGreen
    maxBound = FutuAccent maxBound maxBound

instance NFData Colour
instance Hashable Colour

-- |
--
-- >>> DC.sRGB24show $ colourToDataColour FutuGreen
-- "#329e41"
--
colourToDataColour :: (Floating a, Ord a) => Colour -> DC.Colour a
colourToDataColour = f . colourRGB8
  where
    f (PixelRGB8 r g b) = DC.sRGB24 r g b

-- | Convert to JuicyPixels colour
colourRGB8 :: Colour -> PixelRGB8
colourRGB8 FutuGreen             = PixelRGB8 50 158 65
colourRGB8 FutuBlack             = PixelRGB8 33 15 0
colourRGB8 FutuLightGreen        = PixelRGB8 65 175 70
colourRGB8 FutuDarkGreen         = PixelRGB8 38 104 38
colourRGB8 (FutuAccent AF1 AC1)  = PixelRGB8 205 236 228
colourRGB8 (FutuAccent AF1 AC2)  = PixelRGB8 0 90 75
colourRGB8 (FutuAccent AF1 AC3)  = PixelRGB8 0 52 65
colourRGB8 (FutuAccent AF2 AC1)  = PixelRGB8 190 195 230
colourRGB8 (FutuAccent AF2 AC2)  = PixelRGB8 70 40 154
colourRGB8 (FutuAccent AF2 AC3)  = PixelRGB8 80 10 90
colourRGB8 (FutuAccent AF3 AC1)  = PixelRGB8 238 243 245
colourRGB8 (FutuAccent AF3 AC2)  = PixelRGB8 128 157 175
colourRGB8 (FutuAccent AF3 AC3)  = PixelRGB8 0 31 92
colourRGB8 (FutuAccent AF4 AC1)  = PixelRGB8 255 240 70
colourRGB8 (FutuAccent AF4 AC2)  = PixelRGB8 245 143 145
colourRGB8 (FutuAccent AF4 AC3)  = PixelRGB8 255 82 64
colourRGB8 (FutuAccent AF5 AC1)  = PixelRGB8 242 238 230
colourRGB8 (FutuAccent AF5 AC2)  = PixelRGB8 225 220 220
colourRGB8 (FutuAccent AF5 AC3)  = PixelRGB8 240 214 195
colourRGB8 (FutuAccent AF6 AC1)  = PixelRGB8 255 240 210
colourRGB8 (FutuAccent AF6 AC2)  = PixelRGB8 255 245 175
colourRGB8 (FutuAccent AF6 AC3)  = PixelRGB8 230 245 220
