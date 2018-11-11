module Parse ( requestParser ) where

import Types

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B

import Control.Arrow
import Control.Monad
import Control.Applicative

import Data.Word
import Data.Bits
import Data.Functor

-- Gets the bytes of a 16bit BE word and converts it to
-- a list of Word8s, most significant byte first
bytes16BE :: Word16 -> [Word8]
bytes16BE u16 = [(u16 `shiftR` 8) .&. 0xff, u16 .&. 0xff] <&> fromIntegral

-- Form a Word16 from the most and least significant bytes
fromWordsBE :: Word8 -> Word8 -> Word16
fromWordsBE msb lsb = ((fromIntegral msb) `shiftL` 8) .|. ((fromIntegral lsb) .&. 0xff)

anyWord16BE :: A.Parser Word16
anyWord16BE = liftA2 fromWordsBE A.anyWord8 A.anyWord8

word16BE :: Word16 -> A.Parser ()
word16BE = bytes16BE >>> mapM_ A.word8

ack :: A.Parser Ack
ack = word16BE ackOpcode >> anyWord16BE <&> Ack

requestParser :: A.Parser Packet
requestParser = undefined
