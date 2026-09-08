{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Factory.Droid
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  arguments <- getArgs
  directory <- case arguments of
    [path] -> pure path
    _ -> die "Usage: droid-example WORKING_DIRECTORY (makes two model requests)"
  withDroidSession ((defaultDroidOptions directory) {droidTurnTimeoutMicros = Just 120000000}) $ \session -> do
    void (sendPrompt session "Do not use tools. Reply with exactly HELLO." printChunk)
    Text.putStrLn ""
    void (sendPrompt session "Do not use tools. What word did I ask you to say? Reply with that word only." printChunk)
    Text.putStrLn ""

printChunk :: Text -> IO ()
printChunk text = Text.putStr text >> hFlush stdout
