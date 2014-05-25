 module PNG () where

import Control.Arrow
import Data.ByteString.Lazy as B (ByteString,pack,splitAt,empty,head) 
import Data.Bits
import Data.Char
import Data.Word
import Data.Binary
import Data.Digest.CRC32

import TestData

type Error = String

checkSignature :: ByteString -> Either Error ByteString
checkSignature png = if (pngStart == fileSignature)
               then Right pngRest
               else Left "This doesn't look like a PNG file"
               where (pngStart,pngRest) = B.splitAt 8 png

-- From http://www.w3.org/TR/PNG/#5PNG-file-signature

standardFileSignature :: [Word8]
standardFileSignature = [137,80,78,71,13,10,26,10]

createFileSignature = pack

fileSignature = createFileSignature standardFileSignature

isRight = either (const False) (const True)

chunkify pngStream | pngStream == empty = []
                   | otherwise          = (chunkBody,chunkCRC):chunkify restPNGstream
                   where (chunkBodyLength,png1)   = chunkLength pngStream
                         (chunkBody      ,png2)   = B.splitAt (fromIntegral chunkBodyLength) png1
                         (chunkCRC,restPNGstream) = B.splitAt 4 png2

chunkLength :: ByteString -> (Word32 , ByteString)
chunkLength = B.splitAt 4 >>> first (decode >>> (+4))

-- checkCRC32 :: (ByteString,ByteString) -> Either ByteString ByteString
checkCRC32 (chunkBody,chunkCRC) = if ((crc32 chunkBody) == (decode chunkCRC))
           then Right chunkBody
           else Left  chunkBody

critical = B.head >>> (.&.) 0x20 >>> (==) 0

removeBadAncillery = filter (either critical (const True))

test = checkSignature >>> right (chunkify >>> map checkCRC32 >>> removeBadAncillery)