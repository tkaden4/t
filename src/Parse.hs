{-# LANGUAGE OverloadedStrings #-}

module Parse
    (
        ackParser,
        dataParser,
        writeReqParser,
        readReqParser,
        errorParser,
        packetParser
    ) where

import Types

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B

import Control.Applicative

import Data.Word
import Data.Bits
import Data.Functor
import Data.Tuple

-- Gets the bytes of a 16bit BE word and converts it to
-- a tuple of Word8s, most significant byte first
bytes16BE :: Word16 -> (Word8, Word8)
bytes16BE u16 = (fromIntegral $ (u16 `shiftR` 8) .&. 0xff, u16 .&. 0xff) <&> fromIntegral

-- Form a Word16 from the most and least significant bytes
fromWordsBE :: Word8 -> Word8 -> Word16
fromWordsBE msb lsb = ((fromIntegral msb) `shiftL` 8) .|. ((fromIntegral lsb) .&. 0xff)

-- Parse any valid Word16 in BE format
anyWord16BE :: A.Parser Word16
anyWord16BE = liftA2 fromWordsBE A.anyWord8 A.anyWord8

-- Parse a single Word16 in BE format
word16BE :: Word16 -> A.Parser Word16
word16BE u16 = (mapM A.word8 $ bytes16BE u16) <&> uncurry fromWordsBE

-- Parse an input string + an additional \x00 byte
-- Useful for TFTP options, modes, etc.
nullTermString :: B.ByteString -> A.Parser B.ByteString
nullTermString str = A.string str <* A.word8 0x00

-- Parse any sequence of characters, terminated
-- by an additional \x00 byte. This byte is not
-- included as a result of parsing
anyNullTermString :: A.Parser B.ByteString
anyNullTermString = A.takeTill (== 0x00)

requestModeParser :: A.Parser RequestMode
requestModeParser = mode "netascii" Netascii <|> mode "octet" Octet
    where mode s r = nullTermString s $> r

-- Parse the body of a request
requestBodyParser :: A.Parser Request
requestBodyParser = liftA2 Request anyNullTermString requestModeParser

readReqParser :: A.Parser ReadR
readReqParser = word16BE readRequestOpcode *> requestBodyParser <&> ReadR

writeReqParser :: A.Parser WriteR
writeReqParser = word16BE writeRequestOpcode *> requestBodyParser <&> WriteR


errorCodeParser :: A.Parser ErrorCode
errorCodeParser =
    A.word8 0x00 >>
    A.choice [ w 0x00 NotDefined
             , w 0x01 FileNotFound
             , w 0x02 AccessViolation
             , w 0x03 DiskFull
             , w 0x04 IllegalOperation
             , w 0x05 UnknownTransferID
             , w 0x06 FileExists
             , w 0x07 NoUser
             , w 0x08 OperationFailure ] -- TODO check this one
      where w x r = A.word8 x $> r

-- Parse an error packet
errorParser :: A.Parser Error
errorParser = word16BE errorOpcode *> liftA2 Error errorCodeParser anyNullTermString

ackParser :: A.Parser Ack
ackParser = word16BE ackOpcode *> anyWord16BE <&> Ack

dataParser :: A.Parser Data
dataParser = word16BE dataOpcode *> liftA2 Data anyWord16BE anyNullTermString

-- Bring all the previous parsers together
packetParser :: A.Parser Packet
packetParser =
    A.choice [ ackParser <&> AckP
             , dataParser <&> DataP
             , readReqParser <&> ReadP
             , writeReqParser <&> WriteP
             , errorParser <&> ErrorP ]
