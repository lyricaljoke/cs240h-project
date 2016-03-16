module Audio.Mixer
    ( Mixer
    , getFrame
    , latUs
    ) where

import Data.Array.Unboxed
import Data.Word
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Net.RTP as R
-- FIXME remove TChan, MVar?  Pipes might make them unneeded.
import Control.Concurrent.STM.TChan
import Control.Concurrent.MVar
import Control.Monad.STM
import Audio.Mixer.Utils

type PktType = R.Packet [Word16]

-- TODO add extension header support for source leveling

-- | FIXME
data Mixer =
    Mixer { -- Maximum time the mixer will wait before dropping PktType frame
            -- from PktType particular channel.  This does not include any kind of
            -- rendering / endpoint playback latency.
            acceptableLatencyUs :: Int
            -- FIXME
          , sourceId :: Word32
            -- comms channel for receiving all packets
          , channel :: TChan PktType
            -- FIXME.
          , buffers :: M.Map Word32 [PktType]
            -- Configuration mutex. FIXME - needed?
          , configMutex :: MVar ()
          }

-- | Key = SSRC. Value = number of packets we dropped.
type CullInfo = M.Map Word32 Int

-- | Amount of time to attempt to grab each time.
sliceToGrab :: Mixer -> Int
sliceToGrab mix = (latUs mix) `div` 4

-- | FIXME.
newMixer :: Int -> Word32 -> TChan PktType -> IO Mixer
newMixer lat id chan = Mixer lat id chan M.empty <$> newEmptyMVar

-- | Get the next frame from the mixer.
getFrame :: Mixer -> IO (PktType, Mixer)
getFrame mix = do
    ts <- getSystemTimeUs
    updated <- addNewPackets mix
    let (culled, trimmedMix) = cullOldPackets updated (ts - (fromIntegral $ latUs mix))
    putStrLn $ "Culled " ++ (show $ M.foldr (+) 0 culled) ++
               " old packets from " ++ (show $ L.length $ M.keys culled) ++
               " sources."
    return undefined

ssrcOf = R.ssrc . R.header

-- | FIXME.
addNewPackets :: Mixer -> IO Mixer
addNewPackets mix = do
    newPacket <- atomically $ tryReadTChan (channel mix)
    case newPacket of Just pkt -> addNewPackets
                                    $ mix { buffers = M.insertWith (++) (ssrcOf pkt) [pkt] (buffers mix) }
                      Nothing  -> return mix

-- | FIXME.
cullOldPackets :: Mixer -> Integer -> (CullInfo, Mixer)
cullOldPackets mix cutoffUs =
    (M.empty, mix { buffers = newBuffers} ) where
        newBuffers = snd $ M.mapAccum (\a pkts -> (a, L.filter ((flip isntObsolete) cutoffUs) pkts)) 0 (buffers mix)

isntObsolete :: PktType -> Integer -> Bool
--IntegerisntObsolete p cutoffUs = (timeOfLastSample p) > cutoffUs
isntObsolete p cutoffUs = sampleNumberAtTime cutoffUs p < L.length (R.payload p)

-- FIXME.
fs = 48000 :: Float

sampleNumberAtTime :: Integer -> PktType -> Int
sampleNumberAtTime tUs pkt =
    round $ (fromIntegral (tUs - (fromIntegral $ R.timestamp $ R.header pkt))) * fs / 1000000

-- | Get the acceptable latency value of PktType channel in microseconds.
latUs :: Mixer -> Int
latUs = acceptableLatencyUs

