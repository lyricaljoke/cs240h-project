-- | Represents various test audio sources.
module Audio.Mixer.Sources
    ( Sinusoid
    , getNext
      -- Sinusoid source functions.
    , newSinusoid
    ) where

import Control.Concurrent
import Data.Array.Unboxed
import Data.Word
import qualified Net.RTP as R
import qualified Audio.Mixer.Types as T
import Audio.Mixer.Extensions
import Audio.Mixer.Utils

-- | Represents a stateful sinusoid generator.
data Sinusoid = 
    -- Private constructor.  'newSinusoid' should be used.
    Sinusoid { -- Samples per full period. Should not be modified after
               -- construction.
               sampsPerPeriod :: Int
               -- Sinusoid frequency in Hz.
             , freqHz :: Float
               -- Sample index offset.  This allows us to start the sinusoid
               -- from the correct place in the period when a frame is
               -- requested.
             , sampleIdx :: Integer
               -- Number of packets sent so far.  We use this to populate the
               -- 'sequence' field of the RTP packet.
             , packetCount :: Integer
               -- SSRC number for RTP.
             , sourceId :: Word32
             }

-- FIXME
fs = 48000

pType :: Word8
pType = 0 -- FIXME

-- | Constructs a Sinusoid generator for use with getNext.
newSinusoid :: Float -> Word32 -> IO Sinusoid
newSinusoid fHz ssrc = return $ Sinusoid (round (fs / fHz)) fHz  0 0 ssrc

-- | Gets the next packet from the generator.
-- Args:
--      s : generator
--      samples : number of samples to retrieve
getNext :: Sinusoid -> Int -> IO (T.PktType, Sinusoid)
getNext s samples =
    case s of Sinusoid _ _ _ _ _ -> getSinePacket s samples -- FIXME
              _                  -> undefined -- TODO

-- | Implementation of getNext for the Sinusoid generator.
getSinePacket :: Sinusoid -> Int -> IO (T.PktType, Sinusoid)
getSinePacket s samples = do
    -- TODO -- this is a naive way to simulate a real-time source.  We should
    -- replace with "wait until time point" logic in order not to fall behind
    -- real-time.
    threadDelay $ round ((fromIntegral $ samples * 1000000) / fs)
    let sampOffset = (sampleIdx s) `mod` (fromIntegral (sampsPerPeriod s))
    -- Magic number here just is a reasonable amplitude value (half of max
    -- amplitude for Word16).
    let rawData = map round ([16384.0 * (sin $ 2*pi*(freqHz s) * ((fromIntegral t)/fs)) | t<-(take samples [(-1*sampOffset)..])])
    let ssrc = sourceId s
    -- Populate the standard RTP timestamp...
    timestamp <- fromIntegral <$> getSystemTimeUs
    -- ...but also get a high-precision timestamp.
    (h, l) <- getSystemTimeHighP
    let highPTimestamp = PrecisionTimestamp h l
    let hdr = R.Header 2 False True 0 False pType
                       (fromIntegral $ packetCount s) timestamp ssrc []
    -- Return the packet and update the generator.
    return (R.RawPacket hdr (Just highPTimestamp) rawData,
            Sinusoid (sampsPerPeriod s) (freqHz s) ((sampleIdx s) + (fromIntegral samples)) ((packetCount s) + 1) (sourceId s))

