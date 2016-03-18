{-# LANGUAGE FlexibleInstances #-}
-- | QuickCheck test suite for network-house package.
--
-- Currently, only the RTP packet functionality is tested.

module Main (main) where

import Data.Word
import Data.Maybe (isJust)
import Test.Hspec
import Test.QuickCheck
import qualified Net.Packet as P
import Net.PacketParsing
import qualified Net.RTP as RTP


-- | Test program entry point.
main :: IO ()
main = do
    quickCheck $ property $ (validParsableCorrect :: (StrP -> Bool))
    quickCheck $ property $ (validParsableCorrect :: (NormalExtPkt [Word16] -> Bool))
    quickCheck $ property $ (validParsableCorrect :: (RTP.Header -> Bool))
    quickCheck $ property $ (validParsableCorrect :: (RTP.ExtensionHeader -> Bool))
    -- TODO implement padding!
    -- TODO implement generator specifically for non-extended packets, for better testing?

-- | Many tests aren't particularly concerned with the payload types for
-- the packet and the extension header.  For these, we just use String.
type StrP = NormalExtPkt String 
type NormalExtPkt = RTP.Packet RTP.ExtensionHeader

-- | Verify that we can unparse any validly constructed RTP packet.
validParsableCorrect :: (Parse a, Unparse a, Eq a, Arbitrary a) => a -> Bool
validParsableCorrect p =
    case loopSerialize p of Just looped -> p == looped
                            Nothing     -> False -- Fail if the process fails

-- | Take a packet, serialize it, deserialize it, and get the result.
loopSerialize :: (Parse a, Unparse a, Arbitrary a) => a -> Maybe a
loopSerialize p = doParse $ P.loopback $ doUnparse p

{- The following contains code to implement instance Arbitrary for RTP
   packets. -}

instance (Parse p, Unparse p, Arbitrary p) =>
    Arbitrary (RTP.Packet RTP.ExtensionHeader p) where
        arbitrary = arbRtp

-- | Generate an arbitrary RTP packet with a possible extension header
-- of no particular identifier.
arbRtp :: (Parse p, Unparse p, Arbitrary p) => Gen (NormalExtPkt p)
arbRtp = do
    header <- arbRtpHeader
    ext <- case (RTP.extension header) of
                    True -> Just <$> arbRtpExtensionHeader
                    False -> return Nothing
    content <- arbitrary
    return $ RTP.RawPacket header ext content

instance Arbitrary RTP.Header where
    arbitrary = arbRtpHeader

-- | Generator for the fixed header portion of the RTP packet.
arbRtpHeader :: Gen RTP.Header
arbRtpHeader = do
    version <- choose (0, 3)
    padding <- arbitrary
    extension <- arbitrary
    csrcCount <- choose (0, 15)
    marker <- arbitrary
    payloadType <- choose (0, 127)
    sequenceNum <- arbitrary
    timestamp <- arbitrary
    ssrc <- arbitrary
    csrcs <- vectorOf csrcCount arbitrary
    return $ RTP.Header version padding extension (fromIntegral csrcCount) marker payloadType sequenceNum timestamp ssrc csrcs

instance Arbitrary RTP.ExtensionHeader where
    arbitrary = arbRtpExtensionHeader

-- | Generator for a basic RTP extension header.
arbRtpExtensionHeader :: Gen RTP.ExtensionHeader
arbRtpExtensionHeader = do
    id <- arbitrary
    content <- arbitrary `suchThat` (\l -> P.outLen (doUnparse l) `mod` 4 == 0)
    -- the following division is okay because of the constraint in the previous
    -- line
    let length = fromIntegral $ (P.outLen (doUnparse content)) `div` 4
    return $ RTP.RawExtensionHeader id length content
