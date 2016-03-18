module Audio.Mixer.Sources
    ( Sinusoid
    , getNext
    , killSource
      -- Sinusoid source functions.
    , newSinusoid
    ) where

import Control.Concurrent
import Data.Array.Unboxed
import Data.Word
import qualified Net.RTP as R
import Audio.Mixer.Extensions
import qualified Audio.Mixer.Types as T
import Audio.Mixer.Utils

data Sinusoid = 
    Sinusoid { -- FIXME
               sampsPerPeriod :: Int
               -- FIXME
             , freqHz :: Float
               -- FIXME
             , sampleIdx :: Integer
             , packetCount :: Integer
               -- SSRC number
             , sourceId :: Word32
             }

-- FIXME
fs = 48000

pType :: Word8
pType = 0 -- FIXME

-- | FIXME
newSinusoid :: Float -> Word32 -> IO Sinusoid
newSinusoid fHz ssrc = return $ Sinusoid (round (fs / fHz)) fHz  0 0 ssrc

-- | FIXME
-- FIXME instead of case analysis, use a type class
getNext :: Sinusoid -> Int -> IO (T.PktType, Sinusoid)
getNext s samples =
    case s of Sinusoid _ _ _ _ _ -> getSinePacket s samples -- FIXME
              _                  -> undefined -- TODO

-- | FIXME
-- FIXME all these fromIntegral calls
getSinePacket :: Sinusoid -> Int -> IO (T.PktType, Sinusoid)
getSinePacket s samples = do
    threadDelay $ round ((fromIntegral $ samples * 1000000) / fs) -- FIXME -- we'll fall behind!
    let sampOffset = (sampleIdx s) `mod` (fromIntegral (sampsPerPeriod s))
    let rawData = map round ([16384.0 * (sin $ 2*pi*(freqHz s) * ((fromIntegral t)/fs)) | t<-(take samples [(-1*sampOffset)..])])
    let ssrc = sourceId s -- FIXME
    timestamp <- fromIntegral <$> getSystemTimeUs
    (h, l) <- getSystemTimeHighP
    let highPTimestamp = PrecisionTimestamp h l
    let hdr = R.Header 2 False True 0 False pType (fromIntegral $ packetCount s) timestamp ssrc []
    -- FIXME let ext = Just $ Prec
    return (R.RawPacket hdr (Just highPTimestamp) rawData,
            Sinusoid (sampsPerPeriod s) (freqHz s) ((sampleIdx s) + (fromIntegral samples)) ((packetCount s) + 1) (sourceId s))

-- | FIXME
killSource :: Sinusoid -> IO ()
killSource = undefined

