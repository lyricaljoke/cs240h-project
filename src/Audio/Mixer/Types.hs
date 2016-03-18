module Audio.Mixer.Types
  ( ExtType
  , PayloadType
  , PktType
  , fs
  ) where

import Audio.Mixer.Extensions
import Data.Word
import qualified Net.RTP as R

-- | Types used throughout the audio mixer demo application.
type PayloadType = [Word16]
type ExtType = AudioMixerExtension
type PktType = R.Packet ExtType PayloadType

-- | Sampling rate in Hz.
fs = 48000 :: Float
