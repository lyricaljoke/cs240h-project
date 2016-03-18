module Audio.Mixer.Extensions
  ( AudioMixerExtension(..)
  ) where

import Data.Word
import Net.PacketParsing
import Net.RTP

-- | Collection of application-specific extension header types.
data AudioMixerExtension =
    PrecisionTimestamp { --FIXME
                         highWord :: !Word32
                       , lowWord :: !Word32
                       }

data ExtHeaderId = 
    PTimestamp |
    Unknown
    deriving (Bounded, Enum, Eq, Ord, Show)

instance Parse AudioMixerExtension where
    parse = do
        extId <- word16
        case toEnum (fromIntegral extId) of
            PTimestamp -> do
                check16 2
                PrecisionTimestamp # parse <# parse
            _          -> fail "unrecognized extension"

instance Unparse AudioMixerExtension where
    unparse (PrecisionTimestamp high low) = 
        unparse (fromIntegral (fromEnum PTimestamp) :: Word16) .
        unparse (2 :: Word16) .
        unparse (high, low)

