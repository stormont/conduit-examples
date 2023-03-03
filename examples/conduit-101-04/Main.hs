{-# LANGUAGE OverloadedStrings #-}
module Main
  where

import qualified Data.ByteString.Char8 as B
import Control.Monad.State
import Data.Conduit
import Data.Conduit.Network
import Network.Socket (withSocketsDo)


-- Mostly from: http://www.yesodweb.com/blog/2014/03/network-conduit-async
main = do
  putStrLn "-----------------------------------------------"
  putStrLn "EXAMPLE: Reading from one socket and writing to"
  putStrLn "         another socket"
  putStrLn "-----------------------------------------------"
  putStrLn "Start the example server from conduit-101-02 for writing."
  putStrLn ""
  putStrLn "Press <ENTER> when ready to continue."
  putStrLn ""
  putStr "> "
  _ <- getLine
  putStrLn ""
  putStrLn "Use 'telnet 127.0.0.1 4002' or equivalent on port 4002"
  putStrLn "to read data from the socket."
  putStrLn ""
  putStrLn "Writing to the conduit-101-02 server will continue until"
  putStrLn "a newline is entered."
  putStrLn ""
  putStrLn "Press <Ctrl-C> to stop this server."
  putStrLn ""
  putStr "> "
  withSocketsDo $ do
    runTCPServer (serverSettings 4002 "*") $ \client ->
      runTCPClient (clientSettings 4000 "localhost") $ \server -> runConduit $ do
        appSource client .| echo .| appSink server


echo :: ConduitM B.ByteString B.ByteString IO ()
echo = do
  awaitForever $ \x -> do
    liftIO $ B.putStr x
    yield x

