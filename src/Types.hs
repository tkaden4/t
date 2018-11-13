module Types
    (
        Port,
        TID,
        BlockNumber,
        RequestMode(..),
        Request(..),
        WriteR(..),
        ReadR(..),
        Data(..),
        Ack(..),
        Packet(..),
        ErrorCode(..),
        Error(..),
        readRequestOpcode,
        writeRequestOpcode,
        dataOpcode, 
        ackOpcode,
        errorOpcode
    ) where

import Data.Serialize
import Data.Word
import Data.ByteString

-- TIDs are effectively port numbers
type Port = Word16
type TID = Port

-- Block numbers order sent/received data
type BlockNumber = Word16

-- Requests
data RequestMode = Netascii | Octet
    deriving(Show, Eq)

data Request = Request { path :: ByteString, mode :: RequestMode }
    deriving(Show, Eq)

newtype WriteR = WriteR Request
    deriving(Show, Eq)

newtype ReadR = ReadR Request
    deriving(Show, Eq)

-- Data

data Data = Data { blockNumber :: BlockNumber, info :: ByteString }
    deriving(Show, Eq)

-- Ack

newtype Ack = Ack BlockNumber
    deriving(Show, Eq)

-- Errors

data ErrorCode =
    NotDefined
    | FileNotFound
    | AccessViolation
    | DiskFull
    | IllegalOperation
    | UnknownTransferID
    | FileExists
    | NoUser            -- N/A, part of email (deprecated)
    | OperationFailure  -- N/A, part of options extension (out of scope, but could be added)
    deriving(Show, Eq)


data Error = Error { errno :: ErrorCode, message :: ByteString }
    deriving(Show, Eq)

-- Packet
data Packet =
    AckP Ack
    | DataP Data
    | ReadP ReadR
    | WriteP WriteR
    | ErrorP Error
    deriving(Show, Eq)

readRequestOpcode   = 0x0001 :: Word16
writeRequestOpcode  = 0x0002 :: Word16
dataOpcode          = 0x0003 :: Word16
ackOpcode           = 0x0004 :: Word16
errorOpcode         = 0x0005 :: Word16
