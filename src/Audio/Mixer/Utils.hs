-- | Miscellaneous utility functions used in the audio mixer application.
module Audio.Mixer.Utils
    ( getSystemTimeUs
    , getSystemTimeHighP
    , highPrecToTimestampUs
    ) where

import Data.Bits
import Data.Time.Clock
import Data.Word

-- |Get the current time in microseconds.  This DISCARDS the day data and is
-- a temporary strategy.
getSystemTimeUs :: IO Integer
getSystemTimeUs = toTimestampUs <$> getCurrentTime

-- | Get number of micro ticks; similarly throws away day data.
toTimestampUs :: UTCTime -> Integer
toTimestampUs t = (diffTimeToPicoseconds . utctDayTime $ t) `div` 1000000

-- | Get the current time in nanoseconds, split into two Word32s. Like the
-- previous functions, this discards the day data.
getSystemTimeHighP :: IO (Word32, Word32)
getSystemTimeHighP = toTimestampHighP <$> getCurrentTime

-- | Convert a UTCTime to a particular timestamp format.
toTimestampHighP :: UTCTime -> (Word32, Word32)
toTimestampHighP t =
    let nanos = (fromIntegral (diffTimeToPicoseconds . utctDayTime $ t) `div` 1000) :: Word64
    in (fromIntegral $ nanos `shiftR` 32, fromIntegral $ nanos .&. 0xFFFFFFFF)

-- | Convert back to a more useful format from the split nanosecond timestamp
-- type.
highPrecToTimestampUs :: (Word32, Word32) -> Word64
highPrecToTimestampUs (h, l) =
    (((fromIntegral h :: Word64) `shiftL` 32) + fromIntegral l) `div` 1000
    
