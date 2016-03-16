module Audio.Mixer
    ( Mixer
    , getFrame
    , latUs
    ) where

import Data.Array.Unboxed
import Data.Word
import qualified Data.Map as M
import qualified Net.RTP as R
-- FIXME remove TChan, MVar?  Pipes might make them unneeded.
import Control.Concurrent.STM.TChan
import Control.Concurrent.MVar
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

-- | FIXME.
newMixer :: Int -> Word32 -> TChan PktType -> IO Mixer
newMixer lat id chan = Mixer lat id chan M.empty <$> newEmptyMVar

-- | Get the next frame from the mixer.
getFrame :: Mixer -> IO (PktType, Mixer)
getFrame mix = do
    ts <- getSystemTimeUs
    -- First, discard all packets in all sources that are too old for our latency.
    return undefined
    

timeOfLastSample :: PktType -> Integer
timeOfLastSample = undefined

-- | Get the acceptable latency value of PktType channel in microseconds.
latUs :: Mixer -> Int
latUs = acceptableLatencyUs

