{-# LANGUAGE RankNTypes #-}
module Main
  where

import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.ByteString.Char8 (ByteString, empty, pack, unpack)
import Data.Conduit
import System.IO (stdin)


-- Primary references from:
--   http://www.stackage.org/snapshot/nightly-2015-03-08/package/conduit-1.2.4
--   http://www.stackage.org/snapshot/nightly-2015-03-08/package/conduit-extra-1.1.7.0
-- Bits and pieces from:
--   https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview
--   https://www.fpcomplete.com/user/chad/snippets/random-code-snippets/conduit-from-filepath-to-bytestrings
main = do
  let input = "The quick brown fox jumped over the lazy dog"
  splitWordsExample input
  wordLengthExample_1 input
  wordLengthExample_2 input
  wordLengthExample_3 input
  userInputExample_1
  userInputExample_2
  userInputExample_3
  filesExample_1 "examples/conduit-101-01/words.txt"
  filesExample_2
    "examples/conduit-101-01/words.txt"
    "examples/conduit-101-01/output.txt"
  filesExample_3
    "examples/conduit-101-01/words.txt"
    "examples/conduit-101-01/lengths.txt"


splitWordsExample
  :: String
  -> IO ()
splitWordsExample input = do
  putStrLn "------------------------"
  putStrLn "EXAMPLE: Splitting words"
  putStrLn "------------------------"
  xs <- runConduit $ wordsSource input
     .| identitySink
  mapM_ putStrLn xs
  putStrLn ""


wordLengthExample_1
  :: String
  -> IO ()
wordLengthExample_1 input = do
  putStrLn "-----------------------"
  putStrLn "EXAMPLE: Word lengths 1"
  putStrLn "-----------------------"
  xs <- runConduit $ wordsSource input
     .| numLettersConduit
     .| identitySink
  mapM_ (putStrLn . show) xs
  putStrLn ""


wordLengthExample_2
  :: String
  -> IO ()
wordLengthExample_2 input = do
  putStrLn "-----------------------"
  putStrLn "EXAMPLE: Word lengths 2"
  putStrLn "-----------------------"
  xs <- runConduit $ wordsSource input
     .| numLettersConduit
     .| showConduit
     .| identitySink
  mapM_ putStrLn xs
  putStrLn ""


wordLengthExample_3
  :: String
  -> IO ()
wordLengthExample_3 input = do
  putStrLn "-----------------------"
  putStrLn "EXAMPLE: Word lengths 3"
  putStrLn "-----------------------"
  xs <- runConduit $ wordsSource input
     .| fusedConduit
     .| identitySink
  mapM_ putStrLn xs
  putStrLn ""


userInputExample_1 :: IO ()
userInputExample_1 = do
  putStrLn "-------------------------------"
  putStrLn "EXAMPLE: Splitting user input 1"
  putStrLn "-------------------------------"
  putStrLn "Enter some input."
  putStrLn ""
  putStr "> "
  input <- getLine
  xs <- runConduit $ wordsSource input
     .| identitySink
  mapM_ putStrLn xs
  putStrLn ""


userInputExample_2 :: IO ()
userInputExample_2 = do
  putStrLn "-------------------------------"
  putStrLn "EXAMPLE: Splitting user input 2"
  putStrLn "-------------------------------"
  putStrLn "Enter some input."
  putStrLn ""
  putStr "> "
  _ <- runConduit $ userInputSource
    .| identityStdOutSink
  putStrLn ""


userInputExample_3 :: IO ()
userInputExample_3 = do
  putStrLn "-------------------------------"
  putStrLn "EXAMPLE: Splitting user input 3"
  putStrLn "-------------------------------"
  putStrLn "Enter some input, multiple times."
  putStrLn ""
  putStrLn "Simply hit <ENTER> with no input to quit."
  _ <- runConduit $ userInputConduitSource
    .| identitiesStdOutSink
  putStrLn ""


filesExample_1
  :: FilePath
  -> IO ()
filesExample_1 file = do
  putStrLn "-------------------------------"
  putStrLn "EXAMPLE: Input source from file"
  putStrLn "-------------------------------"
  _ <- runConduitRes $ fileInputSource file
                    .| identitiesResStdOutSink
  putStrLn ""


filesExample_2
  :: FilePath
  -> FilePath
  -> IO ()
filesExample_2 input output = do
  putStrLn "----------------------------"
  putStrLn "EXAMPLE: Output sink to file"
  putStrLn "----------------------------"
  putStrLn $ "See output file: " ++ output
  _ <- runConduitRes $ fileInputSource input
                    .| conduitToByteString
                    .| CB.sinkFile output
  written <- readFile output
  putStrLn written


filesExample_3
  :: FilePath
  -> FilePath
  -> IO ()
filesExample_3 input1 input2 = do
  putStrLn "-----------------------------"
  putStrLn "EXAMPLE: Multiple input files"
  putStrLn "-----------------------------"
  _ <- runConduitRes $ multiFileInputSource [input1, input2]
                    .| conduitFile
                    .| byteStringStdOutSink
  putStrLn ""


wordsSource
  :: Monad m
  => String
  -> forall i. ConduitT i [String] m ()
wordsSource = yield . words


identitySink
  :: Monad m
  => ConduitM [a] o m [a]
identitySink = CL.foldMap id


numLettersConduit
  :: Monad m
  => ConduitT [String] [Int] m ()
numLettersConduit = CL.map (map length)


showConduit
  :: Monad m
  => ConduitT [Int] [String] m ()
showConduit = CL.map (map show)


fusedConduit
  :: Monad m
  => ConduitT [String] [String] m ()
fusedConduit = fuse numLettersConduit showConduit


userInputSource :: forall i. ConduitT i String IO ()
userInputSource = do
  let
    loop acc = do
      c <- liftIO getChar
      case c of
        ' ' -> do
          yield $ reverse acc
          loop []
        '\n' -> yield $ reverse acc
        _ -> loop (c : acc)
  loop []


identityStdOutSink :: forall o. ConduitT String o IO ()
identityStdOutSink = awaitForever $ liftIO . putStrLn


yieldStrings :: ConduitM ByteString [String] IO Bool
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
          yield $ words ss
          return True


userInputConduitSource :: ConduitT () [String] IO ()
userInputConduitSource = do
     CB.sourceHandle stdin
  .| CB.lines
  .| loop
  where
    loop = do
      liftIO $ putStr "> "
      r <- yieldStrings
      case r of
        False -> return ()
        True -> loop


identitiesStdOutSink
  :: forall o. ConduitT [String] o IO ()
identitiesStdOutSink = awaitForever $ mapM_ (liftIO . putStrLn)


fileInputSource
  :: (Monad m, MonadResource m)
  => FilePath
  -> ConduitT () [String] m ()
fileInputSource file = do
     CB.sourceFile file
  .| loop
  where
    loop = do
      mbs <- await
      case mbs of
        Nothing -> return ()
        Just bs -> do
          yield $ words $ unpack bs
          loop


identitiesResStdOutSink
  :: (Monad m, MonadResource m)
  => forall o. ConduitT [String] o m ()
identitiesResStdOutSink = awaitForever $ mapM_ (liftIO . putStrLn)


conduitToByteString
  :: Monad m
  => ConduitT [String] ByteString m ()
conduitToByteString = do
  awaitForever $ yield . pack . unlines


multiFileInputSource
  :: MonadResource m
  => [FilePath]
  -> ConduitT () FilePath m ()
multiFileInputSource files = do
  mapM_ yield files


conduitFile
  :: MonadResource m
  => ConduitT FilePath ByteString m ()
conduitFile = do
  awaitForever CB.sourceFile


byteStringStdOutSink
  :: MonadResource m
  => forall o. ConduitT ByteString o m ()
byteStringStdOutSink = do
  awaitForever $ liftIO . putStrLn . unpack
