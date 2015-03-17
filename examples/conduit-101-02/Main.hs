{-# LANGUAGE OverloadedStrings #-}
module Main
  where

import Control.Monad.State
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Conduit
import Data.Conduit.Network
import Network.Socket (withSocketsDo)


-- Parts from: https://www.fpcomplete.com/user/joehillen/building-an-async-chat-server-with-conduit
main = do
  putStrLn "------------------------------"
  putStrLn "EXAMPLE: Reading from a socket"
  putStrLn "------------------------------"
  putStrLn "Use 'telnet 127.0.0.1 4000' or equivalent on port 4000"
  putStrLn "to read data from the socket."
  putStrLn ""
  putStrLn "Reading from the client will continue until a newline"
  putStrLn "is entered."
  putStrLn "Press <Ctrl-C> to stop server."
  putStrLn ""
  putStr "> "
  withSocketsDo $ do
    runTCPServer (serverSettings 4000 "*") $ \appData -> do
         appSource appData
      $= splitWords
      $$ identitiesStdOutSink


yieldStrings :: ConduitM ByteString String IO Bool
yieldStrings = do
  mbs <- await
  case mbs of
    Nothing -> return False
    Just bs -> do
      let s = takeWhile (\x -> x /= '\r' && x /= '\n')
            $ unpack bs
      case s of
        ""   -> return False
        ss   -> do
          yield ss
          return True


splitWords :: ConduitM ByteString String IO ()
splitWords = do
  loop
  where
    loop = do
      r <- yieldStrings
      case r of
        False -> return ()
        True  -> loop


identitiesStdOutSink
  :: Consumer String IO ()
identitiesStdOutSink = awaitForever $ liftIO . putStr
