-- | Application-specific extension header types for the audio mixer
-- application.
module Audio.Mixer.Extensions
  ( AudioMixerExtension(..)
  ) where

import Data.Word
import Net.PacketParsing
import Net.RTP

-- | Collection of application-specific extension header types.
data AudioMixerExtension =
    -- | Specifies a high precision timestamp in nanoseconds.
    PrecisionTimestamp { -- Most significant word of pair.
                         highWord :: !Word32
                         -- Least significant word of pair.
                       , lowWord :: !Word32
                       }

-- | Mapping from extension header IDs to numeric enum type.
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

