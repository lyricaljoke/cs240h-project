-- | This module defines the audio mixer logic.  It provides an interface to
-- a stateful audio mixer that can mix multiple channels while guaranteeing
-- a maximum latency.
module Audio.Mixer
    ( Mixer
    , getFrame
    , latUs
    , newMixer
    ) where

import Data.Array.Unboxed
import Data.Word
import qualified Data.Map.Strict as M -- FIXMe consider intmap
import qualified Data.List as L
import qualified Net.RTP as R
import Control.Concurrent.STM.TChan
import Control.Concurrent.MVar
import Control.Monad.STM
import Audio.Mixer.Extensions
import qualified Audio.Mixer.Types as T
import Audio.Mixer.Utils

-- | Defines the audio mixer itself.
data Mixer =
    Mixer { -- Maximum time (microseconds) the mixer will wait before dropping
            -- a frame from a particular channel.  This does not include any
            -- kind of rendering / endpoint playback latency.
            acceptableLatencyUs :: Int
            -- Unique source identifier for this mixer.
          , sourceId :: Word32
            -- comms channel for receiving all packets
          , channel :: TChan T.PktType
            -- Mapping from audio channel (identified by its SSRC) to packet
            -- queues.
          , buffers :: M.Map Word32 [T.PktType]
            -- FIXME
          , nextStartTimeUs :: Integer
            -- FIXME
          , packetsSent :: Word16
          }

-- | Key = SSRC. Value = number of packets we dropped.
type CullInfo = M.Map Word32 Int

-- | Amount of time to attempt to grab each time.
sliceToGrab :: Mixer -> Int
sliceToGrab mix = (latUs mix) `div` 4

-- | FIXME.
newMixer :: Int -> Word32 -> TChan T.PktType -> Mixer
newMixer lat id chan = Mixer lat id chan M.empty 0 0

-- | Get the next frame from the mixer.
getFrame :: Mixer -> Int -> IO (T.PktType, Mixer)
getFrame mix samples = do
    ts <- getSystemTimeUs
    let cutoffUs = sliceStart mix ts
    putStrLn $ "current time: " ++ (show ts) ++ "; cutoff: " ++ (show cutoffUs)
    updated <- addNewPackets mix
    let (culled, trimmedMix) = cullOldPackets updated cutoffUs
    putStrLn $ "Culled " ++ (show $ M.foldr (+) 0 culled) ++
               " old packet(s) from " ++ (show $ L.length $ M.keys culled) ++
               " source(s)."
    let base = replicate samples 0
    let (ids, mixed) = M.foldrWithKey (\k pkts (ssrcs, frame) -> (ssrcs ++ [k], addUp frame (getSlice pkts cutoffUs samples))) ([], base) (buffers trimmedMix)
    let nextStartTimeUs = cutoffUs + round (((fromIntegral samples) / fs) * 1000000.0)
    let sequenceNum = packetsSent trimmedMix + 1
    let pkt = R.RawPacket 
                ( R.Header rtpVersion False False (fromIntegral $ length ids) 
                        False payloadType sequenceNum (fromIntegral cutoffUs)
                        (sourceId trimmedMix) ids )
                Nothing -- no extension header for now
                mixed   -- payload
    return (pkt, updateRecords trimmedMix nextStartTimeUs sequenceNum)

addUp :: (Num a) => [a] -> [a] -> [a]
addUp x y = zipWith (+) x y

-- | FIXME.
updateRecords :: Mixer -> Integer -> Word16 -> Mixer
updateRecords mix timestamp packetNum = mix { nextStartTimeUs = timestamp, packetsSent = packetNum }

-- | Using a start time and a number of samples, get a frame from a list of
-- packets. This should only be called on a list of packets that has been
-- culled of old packets (the 'start' timestamp should be within the first
-- packet).
getSlice :: [T.PktType] -> Integer -> Int -> [Word16]
getSlice pkts start samps =
    let startSample = sampleNumberAtTime start (head pkts)
        zeros = repeat 0
        adjStart = max startSample 0
        samples = take (samps - adjStart) ((snd $ L.splitAt adjStart $ L.concatMap R.payload pkts) ++ zeros)
    in (take (samps - (length samples)) zeros) ++ samples

-- Arguments: mixer, current time in microseconds.
sliceStart :: Mixer -> Integer -> Integer
sliceStart mix now = max (now - fromIntegral (latUs mix)) (nextStartTimeUs mix)

-- | FIXME.
ssrcOf = R.ssrc . R.header

-- | FIXME.
addNewPackets :: Mixer -> IO Mixer
addNewPackets mix = do
    newPacket <- atomically $ tryReadTChan (channel mix)
    case newPacket of
        Just pkt -> do
            putStrLn $ "pkt ts: " ++ (show $ getTimestamp pkt)
            addNewPackets $ mix { buffers = M.insertWith (++) (ssrcOf pkt) [pkt] (buffers mix) }
        Nothing  -> return mix

-- | FIXME.
-- FIXME actually populate cull information
cullOldPackets :: Mixer -> Integer -> (CullInfo, Mixer)
cullOldPackets mix cutoffUs =
    let (culled, newBuffers) = M.mapAccumWithKey (cullAccum cutoffUs) M.empty (buffers mix)
    -- Delete any inactive channels from the map.
    in (culled, mix {buffers = M.filter (not . L.null) newBuffers})

-- | FIXME.
cullAccum :: Integer -> CullInfo -> Word32 -> [T.PktType] -> (CullInfo, [T.PktType])
cullAccum cutoffUs info ssrc pkts =
    let (kept, culled) = L.partition ((flip isntObsolete) cutoffUs) pkts
    in (M.insertWith (+) ssrc (length culled) info, kept)

isntObsolete :: T.PktType -> Integer -> Bool
isntObsolete p cutoffUs = sampleNumberAtTime cutoffUs p < L.length (R.payload p)

-- FIXME.
fs = 48000 :: Float
rtpVersion = 2 :: Word8
payloadType = 999

sampleNumberAtTime :: Integer -> T.PktType -> Int
sampleNumberAtTime tUs pkt =
    round $ (fromIntegral (tUs - getTimestamp pkt)) * fs / 1000000

getTimestamp :: T.PktType -> Integer
getTimestamp p =
    case (R.extHeader p) of
        Just (PrecisionTimestamp h l) -> fromIntegral $ highPrecToTimestampUs (h, l)
        _                             -> error "Low precision timestamps not yet supported."

-- | Get the acceptable latency value of T.PktType channel in microseconds.
latUs :: Mixer -> Int
latUs = acceptableLatencyUs

