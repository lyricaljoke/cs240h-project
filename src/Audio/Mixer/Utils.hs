module Audio.Mixer.Utils
    ( getSystemTimeUs
    ) where

import Data.Bits
import Data.Time.Clock
import Data.Word

-- |Get the current time in microseconds.  This DISCARDS the day data! TODO
getSystemTimeUs :: IO Integer
getSystemTimeUs = toTimestampUs <$> getCurrentTime

-- | Get number of micro ticks; similarly throws away day data.
toTimestampUs :: UTCTime -> Integer
toTimestampUs t = (diffTimeToPicoseconds . utctDayTime $ t) `div` 1000000

getSystemTimeHighP :: IO (Word32, Word32)
getSystemTimeHighP = toTimestampHighP <$> getCurrentTime

toTimestampHighP :: UTCTime -> (Word32, Word32)
toTimestampHighP t =
    let nanos = (fromIntegral (diffTimeToPicoseconds . utctDayTime $ t) `div` 1000) :: Word64
    in (fromIntegral $ nanos `shiftR` 32, fromIntegral $ nanos .&. 0xFFFFFFFF)


