-- Copied lovingly from https://wiki.haskell.org/Implement_a_chat_server.
-- FIXME modify!
module Main where
 
import Network --connectTo, PortID
import System.IO
import Control.Exception
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TChan
import Control.Monad.Fix (fix)
import Control.Monad.STM (atomically)
import Data.Word
import Data.List (elemIndex)
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array
import System.Environment (getArgs)
import Sound.Pulse.Simple
import Audio.Mixer
import Audio.Mixer.Sources
import qualified Audio.Mixer.Types as T
import qualified Net.RTP as R
import qualified Net.PacketParsing as P
import Net.Packet(Chunk, chunks, OutPacket, outLen, toInPack)
import Data.Array.Unboxed(bounds, listArray)
import Data.Array.IO
import Data.Array.MArray

-- | Audio mix server / client entry point. 
main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left msg -> do
            putStrLn $ "Failed parsing arguments: " ++ msg
            doUsage
        Right (ClientParams h p s f) -> runClient h p s f
        Right (HostParams p s)     -> runServer p s

-- | Print program usage.
doUsage :: IO ()
doUsage = do
    putStrLn "Usage:"
    putStrLn "pulse-test [-c] [-s] [-h hostname] [-p port] [-ssrc source-id] [-f freq (Hz)]"
    putStrLn "-c = client mode; hostname, port, source id and frequency required"
    putStrLn "-s = server mode; port and source id argument required"

-- | Data type for containing command line options.
 -- TODO change these to PortIDs or PortNumbers?
data RunParams = ClientParams { host :: String
                              , port :: Word16 
                              , ssrc :: Word32 
                              , freq :: Float  }
               | HostParams   { port :: Word16 
                              , ssrc :: Word32 }
    deriving (Eq, Show)

-- | Client mode entry point.
-- Args:
--      h: hostname
--      p: port number
--      s: unique SSRC for RTP
--      f: frequency (Hz) of test sinusoid
runClient :: String -> Word16 -> Word32 -> Float -> IO ()
runClient h p s f = do
    putStrLn $ "Connecting to " ++ h ++ " on port " ++ (show p)
    hdl <- connectTo h (PortNumber $ fromIntegral p)
    hSetBuffering hdl NoBuffering
    hSetBinaryMode hdl True
    sinSource <- newSinusoid f s
    putStrLn "constructed sinusoid."
    loop hdl sinSource where
        loop = \hdl source -> do
            putStrLn "getting next..."
            (pkt, source') <- getNext source 24000
            let serialized = P.doUnparse pkt
            putStrLn $ "Sending packet with size " ++ (show $ outLen serialized) ++ " bytes."
            writeOutPacket hdl (chunks serialized)
            loop hdl source'

-- | Write a collection of Chunks to a given handle.
writeOutPacket :: Handle -> [Chunk] -> IO ()
writeOutPacket _ [] = return ()
writeOutPacket hdl (x:xs) = do
    arr <- thaw x
    hPutArray hdl arr (arraySize x)
    writeOutPacket hdl xs

-- | Determine the size of a chunk (immutable array).
arraySize :: Chunk -> Int
arraySize a = max - min + 1
    where
        (min, max) = bounds a

-- | Parse the input arguments.  Failure occurs if any required arg is missing
-- for the given mode, or if the mode (client or server) cannot be determined.
parseArgs :: [String] -> Either String RunParams
parseArgs args =
    case (elem "-c" args, elem "-s" args) of
        (True, True) -> Left "cannot specify both \"-c\" and \"-s\" arguments"
        (False, False) -> Left "no run mode specified"
        (True, _) -> parseClientArgs args
        (_, True) -> parseHostArgs args

parseClientArgs :: [String] -> Either String RunParams
parseClientArgs args = do
    h <- argAfter "-h" args
    p <- argAfter "-p" args
    s <- argAfter "-ssrc" args
    f <- argAfter "-f" args
    return $ ClientParams h (read p) (read s) (read f)

parseHostArgs :: [String] -> Either String RunParams
parseHostArgs args = do
    p <- argAfter "-p" args
    s <- argAfter "-ssrc" args
    return $ HostParams (read p) (read s)

-- | Retrieve the argument after a given flag in a list.  Failure occurs if
-- the flag is missing or if it occurs only at the end of the list.
argAfter ::  String -> [String] -> Either String String
argAfter flag args =
    case elemIndex flag args of
        Nothing -> Left $ "required flag missing: " ++ flag
        Just idx -> next where
            next | idx == (length args) =  Left $ "Missing argument for flag " ++ flag
                 | otherwise = Right (args !! (idx + 1))

-- | Server mode loop entry point.  Accepts connections, adds them to the
-- active channel, and begins a mixing / playback thread.
-- Args:
--      p: port number to accept connections on
--      s: unique SSRC identifier to sign output packets with
runServer :: Word16 -> Word32 -> IO ()
runServer p s = do
    sock <- listenOn (PortNumber $ fromIntegral p)
    chan <- newTChanIO
    -- Instantiate a handle for simple mono audio playback.
    player <- simpleNew Nothing "player" Play Nothing "Player for mixed audio"
              monoPcm16 Nothing Nothing
    -- Begin the mixing / playback thread.
    forkIO $ playbackLoop player (newMixer 1000000 s chan)
    mainLoop sock chan 0
    simpleFree player

-- | Mixing / playback thread entry points.  Simply grabs fixed-sized frames
-- from the audio mixer and plays them back.
playbackLoop :: Simple -> Mixer -> IO ()
playbackLoop player mixer = do
        (msg, mixer') <- getFrame mixer 12000
        playAudio player msg
        playbackLoop player mixer'

-- | SampleSpec for playback.  Little endian signed PCM16; single channel.
monoPcm16 = SampleSpec (S16 LittleEndian) 48000 1

-- | Main server loop entry point.
-- Args:
--      sock: socket to accept connections on
--      chan: singleton packet channel to write incoming packets to
--      msgNum: FIXME remove
mainLoop :: Socket -> TChan T.PktType -> Int -> IO ()
mainLoop sock chan msgNum = do
  (hdl, h, p) <- accept sock
  putStrLn $ "New connection from " ++ h ++ ":" ++ (show p)
  forkIO (runConn hdl chan msgNum)
  mainLoop sock chan $! msgNum + 1
 
-- | Connection handler for server mode.
-- Args:
--      hdl: handle to connection
--      chan: packet channel to write incoming RTP packets to
--      msgNum: FIXME remove
runConn :: Handle -> TChan T.PktType -> Int -> IO ()
runConn hdl chan msgNum = do
    let broadcast msg = writeTChan chan msg
    hSetBuffering hdl NoBuffering
    hSetBinaryMode hdl True

    commLine <- atomically $ dupTChan chan
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        -- New immutable array for receiving binary data off the socket.
        let inArray = listArray (0, 48023) (take 48024 (repeat 0)) :: Chunk
        -- Mutate by reading in binary data.
        mutArray <- thaw inArray
        bytesRead <- hGetArray hdl mutArray 48024
        filled <- toInPack <$> freeze mutArray
        case bytesRead of
            48024 -> do
                -- Parse what we've just read in.
                case P.doParse filled of
                    Just pkt -> (atomically $ broadcast pkt) >> loop
                    Nothing  -> putStrLn $ "Error occurred parsing RTP packet."
            _     -> putStrLn $ "An error occurred: only read " ++ (show bytesRead) ++ " bytes."
 
    putStrLn "Closing connection."
    hClose hdl                             -- close the handle

-- | Play back audio from a packet. This blocks until the packet is "nearly"
-- played back.
-- Args:
--      player: endpoint audio renderer handle
--      p: packet to play audio from
playAudio :: Simple -> T.PktType -> IO ()
playAudio player p = do
    putStrLn $ "playing packet with length " ++ (show $ length $ R.payload p)
    simpleWrite player (R.payload p)

