{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
-- | Common integrations requests.
module Case.Integrations.Common (
    ) where
import Data.Time
       (Day (..), LocalTime (..), NominalDiffTime, TimeOfDay (..), UTCTime (..))
import Data.Time
       (addDays, addGregorianMonthsClip, fromGregorian, toGregorian)
import Data.Time.Calendar.WeekDate   (toWeekDate)
import Data.Time.TH                (mkDay, mkUTCTime)


-- |
--
-- >>> beginningOfCurrMonth $(mkDay "2016-11-12")
-- 2016-11-01
beginningOfCurrMonth :: Day -> Day
beginningOfCurrMonth = fromGregorian' . f. toGregorian
  where
    f (y, m, _) = (y, m, 1)

    fromGregorian' :: (Integer, Int, Int) -> Day
    fromGregorian' (y, m, d) = fromGregorian y m d

-- |
--
-- >>> beginningOfPrevMonth $(mkDay "2016-11-12")
-- 2016-10-01
beginningOfPrevMonth :: Day -> Day
beginningOfPrevMonth = addGregorianMonthsClip (-1) . beginningOfCurrMonth

-- |
--
-- >>> endOfPrevMonth $(mkDay "2016-11-12")
-- 2016-10-31
endOfPrevMonth :: Day -> Day
endOfPrevMonth = pred . beginningOfCurrMonth

-- |
--
-- >>> beginningOfPrev2Month $(mkDay "2016-11-12")
-- 2016-09-01
beginningOfPrev2Month :: Day -> Day
beginningOfPrev2Month = addGregorianMonthsClip (-2) . beginningOfCurrMonth

-- |
--
-- @2018-08-10@ is Friday:
--
-- >>> map previousFriday [ $(mkDay "2018-08-09") .. $(mkDay "2018-08-12") ]
-- [2018-08-03,2018-08-03,2018-08-10,2018-08-10]
--
-- >>> previousFriday $(mkDay "2018-08-10")
-- 2018-08-03
--
-- >>> previousFriday $(mkDay "2018-08-11")
-- 2018-08-10
--
previousFriday :: Day -> Day
previousFriday d
    | wd >= 6   = addDays (fromIntegral $ 5 - wd) d
    | otherwise = addDays (fromIntegral $ -2 - wd) d
  where
    (_, _, wd) = toWeekDate d

-- $setup
-- >>> :set -XTemplateHaskell
