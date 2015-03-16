{-# LANGUAGE OverloadedStrings #-}
module Main
  where

import Control.Monad.State
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Conduit
import Data.Conduit.Network
import Network.Socket (withSocketsDo)


main = do
  putStrLn "------------------------------"
  putStrLn "EXAMPLE: Writing to a socket"
  putStrLn "------------------------------"
  putStrLn "Start the example server from conduit-101-02."
  putStrLn ""
  putStrLn "Press <ENTER> when ready to continue."
  putStr "> "
  _ <- getLine
  withSocketsDo $ do
    runTCPClient (clientSettings 4000 "localhost") $ \server -> do
         yield "hello world\n"
      $$ appSink server
