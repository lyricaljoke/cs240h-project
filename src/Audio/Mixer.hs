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
import Audio.Mixer.Utils

type PktType = R.Packet [Word16]

-- TODO add extension header support for source leveling

-- | FIXME
data Mixer =
    Mixer { -- Maximum time the mixer will wait before dropping PktType frame
            -- from PktType particular channel.  This does not include any kind of
            -- rendering / endpoint playback latency.  FIXME consider Integer
            acceptableLatencyUs :: Int
            -- FIXME
          , sourceId :: Word32
            -- comms channel for receiving all packets
          , channel :: TChan PktType
            -- FIXME.
          , buffers :: M.Map Word32 [PktType]
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
newMixer :: Int -> Word32 -> TChan PktType -> Mixer
newMixer lat id chan = Mixer lat id chan M.empty 0 0

-- | Get the next frame from the mixer.
getFrame :: Mixer -> Int -> IO (PktType, Mixer)
getFrame mix samples = do
    ts <- getSystemTimeUs
    let cutoffUs = sliceStart mix ts
    putStrLn $ "current time: " ++ (show ts) ++ "; cutoff: " ++ (show cutoffUs)
    updated <- addNewPackets mix
    let (culled, trimmedMix) = cullOldPackets updated cutoffUs
    putStrLn $ "Culled " ++ (show $ M.foldr (+) 0 culled) ++
               " old packet(s) from " ++ (show $ L.length $ M.keys culled) ++
               " source(s)."
    let base = take samples (repeat 0)
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
addUp x y = [(x' + y') | x' <- x, y' <- y] 

-- | FIXME.
updateRecords :: Mixer -> Integer -> Word16 -> Mixer
updateRecords mix timestamp packetNum = mix { nextStartTimeUs = timestamp, packetsSent = packetNum }

-- | Using a start time and a number of samples, get a frame from a list of
-- packets. This should only be called on a list of packets that has been
-- culled of old packets (the 'start' timestamp should be within the first
-- packet).
getSlice :: [PktType] -> Integer -> Int -> [Word16]
getSlice pkts start samps =
    let startSample = sampleNumberAtTime start (head pkts)
        adjStart = max startSample 0
        samples = take (samps - adjStart) ((snd $ L.splitAt adjStart $ L.concatMap R.payload pkts) ++ repeat 0)
    in (take (samps - (length samples)) $ repeat 0) ++ samples

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
            putStrLn $ "pkt ts: " ++ (show $ R.timestamp $ R.header pkt)
            addNewPackets $ mix { buffers = M.insertWith (++) (ssrcOf pkt) [pkt] (buffers mix) }
        Nothing  -> return mix

-- | FIXME.
-- FIXME actually populate cull information
cullOldPackets :: Mixer -> Integer -> (CullInfo, Mixer)
cullOldPackets mix cutoffUs =
    let (culled, newBuffers) = M.mapAccumWithKey (cullAccum cutoffUs) M.empty (buffers mix)
    -- Delete any inactive channels from the map.
    in (culled, mix {buffers = M.filter (not . L.null) newBuffers})
    --(M.empty, mix { buffers = newBuffers } ) where
    --    newBuffers = 

-- | FIXME.
cullAccum :: Integer -> CullInfo -> Word32 -> [PktType] -> (CullInfo, [PktType])
cullAccum cutoffUs info ssrc pkts =
    let (kept, culled) = L.partition ((flip isntObsolete) cutoffUs) pkts
    in (M.insertWith (+) ssrc (length culled) info, kept)

isntObsolete :: PktType -> Integer -> Bool
isntObsolete p cutoffUs = sampleNumberAtTime cutoffUs p < L.length (R.payload p)

-- FIXME.
fs = 48000 :: Float
rtpVersion = 2 :: Word8
payloadType = 999


sampleNumberAtTime :: Integer -> PktType -> Int
sampleNumberAtTime tUs pkt =
    round $ (fromIntegral (tUs - (fromIntegral $ R.timestamp $ R.header pkt))) * fs / 1000000

-- | Get the acceptable latency value of PktType channel in microseconds.
latUs :: Mixer -> Int
latUs = acceptableLatencyUs

