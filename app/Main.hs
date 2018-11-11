{-# Language TemplateHaskell #-}

module Main where

import qualified TFTP 


main :: IO ()
main = TFTP.runServer config
    where config = TFTP.ServerConfig 7000 $(TFTP.mkRelDir ".")
