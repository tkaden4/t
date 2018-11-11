{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExistentialQuantification #-}

module TFTP
    (
        ServerConfig(..),
        runServer,
        module Types,
        module Parse
    ) where

import Types
import Parse

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad

import Data.Functor

import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NetB

import Path

-- Server configuration
-- TODO change root to Path
data ServerConfig = forall b. ServerConfig { port :: Port, rootDir :: Path b Dir } 

-- Maximum UDP packet size (TFTP requests must fit into a buffer of this size)
maxPacketSize = 65507

-- Server Logic
runServer :: ServerConfig -> IO ()
runServer ServerConfig { port, rootDir } =
    bracket openSocket Net.close listenOnSock
  where openSocket = Net.socket Net.AF_INET Net.Datagram Net.defaultProtocol
        listenOnSock sock =
            do Net.setSocketOption sock Net.ReusePort 1
               putStrLn $ "Listening on port " ++ show port ++ " with root dir " ++ show rootDir
               Net.bind sock $ Net.SockAddrInet (fromInteger $ toInteger port) $ Net.tupleToHostAddress (0, 0, 0, 0)
               forever $ do
                   s <- NetB.recvFrom sock maxPacketSize
                   -- TODO remove this dummy code
                   putStrLn $ show s
