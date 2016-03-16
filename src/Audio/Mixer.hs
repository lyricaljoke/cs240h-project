module Audio.Mixer
    ( Mixer
    , getFrame
    , latUs
    ) where

import Data.Array.Unboxed
import Data.Word
import qualified Data.Map as M
import qualified Net.RTP as R
-- FIXME remove Chan, MVar?  Pipes might make them unneeded.
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Audio.Mixer.Utils

type PktType = R.Packet [Word16]

-- | FIXME
data Mixer =
    Mixer { -- Maximum time the mixer will wait before dropping PktType frame
            -- from PktType particular channel.  This does not include any kind of
            -- rendering / endpoint playback latency.
            acceptableLatencyUs :: Int
            -- FIXME
          , sourceId :: Word32
            -- comms channel for receiving all packets
          , channel :: Chan PktType
            -- FIXME.
          , buffers :: M.Map Word32 [PktType]
            -- Configuration mutex. FIXME - needed?
          , configMutex :: MVar ()
          }

-- | FIXME.
newMixer :: Int -> Word32 -> Chan PktType -> IO Mixer
newMixer lat id chan = Mixer lat id chan M.empty <$> newEmptyMVar

-- | Get the next frame from the mixer.
getFrame :: Mixer -> IO (PktType, Mixer)
getFrame mix = do
    ts <- getSystemTimeUs
    return undefined

-- | Get the acceptable latency value of PktType channel in microseconds.
latUs :: Mixer -> Int
latUs = acceptableLatencyUs

