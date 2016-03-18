-- | Module for parsing RTP packets.
module Net.RTP
    ( ExtensionHeader(..)
    , Header(..)
    , Packet(..)
    ) where

-- Real-time transport protocol (RTP).
-- See https://www.ietf.org/rfc/rfc3550.txt

import Net.Bits
import Net.PacketParsing
import Net.Utils(Container(..))

-- | RTP packet type.
-- p for payload type; e for extension header type.
data Packet e p = RawPacket { header :: !Header
                            , extHeader :: !(Maybe e) -- TODO strictness not applying deeply?
                            , payload :: !p }
    deriving (Eq, Show)

-- | Unparse instance for RTP packet; simply unparse present components.
instance (Unparse p, Unparse e) => Unparse (Packet e p) where
    unparse (RawPacket h e p) =
        case e of Just v -> unparse (h, v, p)
                  _      -> unparse (h, p)

-- | Other standard typeclass instances for RTP packet.
instance Functor   (Packet a) where fmap f p = p { payload = f (payload p) }
instance Container (Packet a) where contents = payload

-- | RTP header.
-- TODO -- this header contains redudancies that should be removed, including:
--      - the 'extension' field; this is indicated by whether the extHeader
--        of the overall packet is present
--      - the csrcCount field; this is equivalent to the length of csrcs
data Header =
    Header { version :: !Word8 
    -- TODO - padding is not currently supported (in serialization and
    -- deserialization, no padding will be added or expected, respecively)
           , padding :: !Bool
           , extension :: !Bool
           , csrcCount :: !Word8
           , marker :: !Bool
           , payloadType :: !Word8
           , sequenceNum :: !Word16
           , timestamp :: !Word32
           , ssrc :: !Word32
           , csrcs :: ![Word32] }
    deriving (Eq, Show)

-- | Extension header.
-- TODO -- this header contains redundancies that should be removed.  It is
-- possible to compute the length while unparsing, and during parsing it
-- shouldn't be necessary to save the value into a software-accessible form.
data ExtensionHeader =
    RawExtensionHeader { identifier :: !Word16
                       , len :: !Word16
    -- If a specific extension header type is not defined, we leave handling
    -- the actual ExtensionHeader content to the user of the network-house
    -- library.
                       , content :: ![Word32] }
    deriving (Eq, Show)

-- | Parse instance for RTP packet.
instance (Parse p, Parse e) => Parse (Packet e p) where
    parse = do
        header <- parse
        case (extension header) of
            True  -> do
                ext <- parse
                pl <- parse
                return $ RawPacket header (Just ext) pl
            False -> do
                pl <- parse
                return $ RawPacket header Nothing pl

instance Parse Header where
    parse = do
        version' <- bits 2
        padding' <- parse
        extension' <- parse
        csrcCount' <- bits 4
        marker' <- parse
        payloadType' <- bits 7
        sequence' <- parse
        timestamp' <- parse
        ssrc' <- parse
        csrcs' <- parses $ fromIntegral csrcCount'
        return $ Header version' padding' extension' csrcCount' marker'
                        payloadType' sequence' timestamp' ssrc' csrcs'

instance Parse ExtensionHeader where
    parse = do
        id <- parse
        length' <- parse
        content' <- parses $ fromIntegral length'
        return $ RawExtensionHeader id length' content'

-- | Repeat a particular parser 'n' times.
parses :: (Parse a) => Int -> PacketParser [a]
parses n = sequence (replicate n parse)

-- | Public constructor.  This should be removed once the actual types don't
-- include redundant information.
packet :: (Parse p, Parse e) => Header -> Maybe e -> p -> Packet e p
packet = RawPacket

instance Unparse Header where
    unparse (Header v p e cn m pt s t ss cs) =
        unparse ((v << 6 .|. (toBits p) << 5 .|. (toBits e) << 4 .|. cn)::Word8) .
        unparse (((toBits m) << 7 .|. pt)::Word8) .
        unparse s .
        unparse t .
        unparse ss .
        unparse cs
        where
            (<<) = shiftL
            toBits = fromIntegral . fromEnum

instance Unparse ExtensionHeader where
    unparse (RawExtensionHeader id l content) =
        unparse (id, l, content)

