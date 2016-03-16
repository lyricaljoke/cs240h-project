module Audio.Mixer.Utils
    ( getSystemTimeUs
    ) where

import Data.Time.Clock

-- |Get the current time in microseconds.  This DISCARDS the day data! TODO
getSystemTimeUs :: IO Integer
getSystemTimeUs = toTimestampUs <$> getCurrentTime

-- | Get number of micro ticks; similarly throws away day data.
toTimestampUs :: UTCTime -> Integer
toTimestampUs t = (diffTimeToPicoseconds . utctDayTime $ t) `div` 1000000
