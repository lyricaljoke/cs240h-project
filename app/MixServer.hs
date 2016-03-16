-- Copied lovingly from https://wiki.haskell.org/Implement_a_chat_server.
-- FIXME modify!
module Main where
 
-- FIXME choose ONE of these network things
import Network --connectTo, PortID
--import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TChan
import Control.Monad (liftM, when)
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
-- FIXME remove; put in lib only!
import qualified Net.RTP as R
import qualified Net.PacketParsing as P
import Net.Packet(Chunk, chunks, OutPacket, outLen, toInPack)
import Data.Array.Unboxed(bounds, listArray)
import Data.Array.IO
import Data.Array.MArray
 
main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left msg -> do
            putStrLn $ "Failed parsing arguments: " ++ msg
            doUsage
        Right (ClientParams h p s) -> runClient h p s
        Right (HostParams p s)     -> runServer p s

-- | Print program usage.
doUsage :: IO ()
doUsage = do
    putStrLn "Usage:"
    putStrLn "pulse-test [-c] [-s] [-h hostname] [-p port] [-ssrc source-id]"
    putStrLn "-c = client mode; hostname, port, and source id required for initial connection"
    putStrLn "-s = server mode; port and source id argument required"

data RunParams = ClientParams { host :: String
                              , port :: Word16  -- FIXME change these to PortIDs or PortNumbers?
                              , ssrc :: Word32 }
               | HostParams   { port :: Word16 
                              , ssrc :: Word32 }
    deriving (Eq, Show)

-- |FIXME.
runClient :: String -> Word16 -> Word32 -> IO ()
runClient h p s = do
    putStrLn $ "Connecting to " ++ h ++ " on port " ++ (show p)
    hdl <- connectTo h (PortNumber $ fromIntegral p) -- FIXME FIXME
    hSetBuffering hdl NoBuffering
    hSetBinaryMode hdl True
    sinSource <- newSinusoid 256.0 s
    putStrLn "constructed sinusoid."
    loop hdl sinSource where
        loop = \hdl source -> do
            putStrLn "getting next..."
            (pkt, source') <- getNext source 24000
            let serialized = P.doUnparse pkt
            putStrLn $ "Sending packet with size " ++ (show $ outLen serialized) ++ " bytes."
            writeOutPacket hdl (chunks serialized)
            loop hdl source'

writeOutPacket :: Handle -> [Chunk] -> IO ()
writeOutPacket _ [] = return ()
writeOutPacket hdl (x:xs) = do
    arr <- thaw x
    hPutArray hdl arr (arraySize x)
    writeOutPacket hdl xs

-- FIXME move this
arraySize :: Chunk -> Int
arraySize a = max - min + 1
    where
        (min, max) = bounds a

{-
    capture <- simpleNew Nothing "capture" Record Nothing "client audio capture" stereoPcm16 Nothing Nothing
    fix $ \loop -> do
        audio <- simpleRead capture 48000
        let audioPacket = [fromIntegral (length audio) :: Word16] ++ audio
        let l = length audioPacket
        putStrLn $ "writing packet of length " ++ (show l)
        withArray audioPacket (flip (hPutBuf hdl) (2*l)) -- FIXME replace 2 with sizeof or something
        --threadDelay $ 500 * 1000
        loop
-}

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
    return $ ClientParams h (read p) (read s)

parseHostArgs :: [String] -> Either String RunParams
parseHostArgs args = do
    p <- argAfter "-p" args
    s <- argAfter "-ssrc" args
    return $ HostParams (read p) (read s)

argAfter ::  String -> [String] -> Either String String
argAfter flag args =
    case elemIndex flag args of
        Nothing -> Left $ "required flag missing: " ++ flag
        Just idx -> next where
            next | idx == (length args) =  Left $ "Missing argument for flag " ++ flag
                 | otherwise = Right (args !! (idx + 1))

runServer :: Word16 -> Word32 -> IO ()
runServer p s = do
--  sock <- socket AF_INET Stream 0
--  setSocketOption sock ReuseAddr 1
--  bind sock (SockAddrInet port iNADDR_ANY)
    sock <- listenOn (PortNumber $ fromIntegral p) -- FIXME consider converting to PortNumber earlier
    chan <- newTChanIO
    player <- simpleNew Nothing "player" Play Nothing "Player for mixed audio" stereoPcm16 Nothing Nothing
    forkIO $ fix $ \loop -> do
        msg <- atomically $ readTChan chan
        -- FIXME this strategy only works for one connected client at a time at the moment
        playAudio player msg
        loop
    mainLoop sock chan 0
    simpleFree player

stereoPcm16 :: SampleSpec
stereoPcm16 = SampleSpec (S16 LittleEndian) 48000 2

monoPcm16 = SampleSpec (S16 LittleEndian) 48000 1

monoPcm16Cd = SampleSpec (S16 LittleEndian) 44100 1

stereoPositions :: [ChannelPosition]
stereoPositions = [(ChannelNormal PanLeft), (ChannelNormal PanRight)]

type Msg = (Int, String)
type Packet = (Int, [Word16])
type RawPacket = (Int, [Word16])

-- |FIXME for now, always have half-second long stereo packets.
packLen :: Word16
packLen = 48000

mainLoop :: Socket -> TChan (R.Packet [Word16]) -> Int -> IO ()
mainLoop sock chan msgNum = do
  -- FIXME use host and port?
  (hdl, h, p) <- accept sock
  putStrLn $ "New connection from " ++ h ++ ":" ++ (show p)
  forkIO (runConn hdl chan msgNum)
  mainLoop sock chan $! msgNum + 1
 
-- | FIXME remove.
testPacket :: [Word16]
testPacket = [packLen] ++ [0..(packLen - 1)]

runConn :: Handle -> TChan (R.Packet [Word16]) -> Int -> IO ()
runConn hdl chan msgNum = do
    let broadcast msg = writeTChan chan msg
--    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hSetBinaryMode hdl True

    commLine <- atomically $ dupTChan chan

    -- fork off a thread for reading from the duplicated channel
{-    reader <- forkIO $ fix $ \loop -> do
        (nextNum, audio) <- readChan commLine
        when (msgNum /= nextNum) $ playAudio audio
        loop
-} 
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        --rawPacket <- allocaArray actualPackLen (readPacket hdl 48012) -- FIXME ugly int cast
        let inArray = listArray (0, 48011) (take 48012 (repeat 0)) :: Chunk
        mutArray <- thaw inArray
        bytesRead <- hGetArray hdl mutArray 48012
        filled <- toInPack <$> freeze mutArray
        case bytesRead of
            48012 -> do
                case P.doParse filled of
                    Just pkt -> (atomically $ broadcast pkt) >> loop
                    Nothing  -> putStrLn $ "Error occurred parsing RTP packet."
            _     -> putStrLn $ "An error occurred: only read " ++ (show bytesRead) ++ " bytes."
             -- If an exception is caught, send a message and break the loop
             -- "quit" -> hPutStrLn hdl "Bye!"
             -- else, continue looping.
             -- _      -> broadcast (name ++ ": " ++ line) >> loop
 
    --killThread reader                      -- kill after the loop ends
    putStrLn "Closing connection."
    hClose hdl                             -- close the handle

-- |Read a packet of length 'n' of types 'a' from the handle into the buffer
-- 'buf'.
--readPacket :: (Storable a) => Handle -> Int -> Ptr a -> IO (Either String [a])
-- FIXME find out a way to keep this generic.  Type inference at call-site?
readPacket :: Handle -> Int -> Ptr Word16 -> IO (Either String [Word16])
readPacket hdl n buf = do
    bytesRead <- hGetBuf hdl buf (n * 2)
    --case bytesRead of
    -- FIXME this indentation
    ret bytesRead buf where
        ret b buffer | b == (n * 2) = Right <$> peekArray n buffer -- FIXME 2 magic number (need sizeof)
                     | otherwise = return (Left $ ("only read " ++ (show b) ++ " bytes"))

-- |FIXME not really implemented.
playAudio :: Simple -> R.Packet [Word16] -> IO ()
playAudio player p = do
    putStrLn $ "Got packet " ++ (show $ R.sequenceNum $ R.header p) ++ " from source " ++ (show $ R.ssrc $ R.header p) ++ "."
    --threadDelay 500000 -- Half a second.  FIXME.
    --simpleWrite player p

