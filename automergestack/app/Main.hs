{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Data.List.Split (splitOn)
import Data.Text (pack, strip, unpack)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Process (callCommand, readCreateProcess, runCommand, shell)

oneSecond :: Int
oneSecond = 1000000

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse [] = do
  putStrLn "You must provide a revset to this script"
  exitFailure
parse [revset] = do
  bookmarks <- getOrderedBookmarksForRevset revset
  mergeAllBookmarks bookmarks Nothing
  exitFailure
parse _ = do
  putStrLn "Too many arguments, this function only accepts one revset"
  exitFailure

mergeAllBookmarks :: [String] -> Maybe String -> IO ()
mergeAllBookmarks [] _ = exitSuccess
mergeAllBookmarks (bookmark : remaining) maybeRebaseChangeId = do
  case maybeRebaseChangeId of
    Just rebaseChangeId -> pullAndRebaseStack rebaseChangeId
    Nothing -> pure ()
  ready <- isBookmarkReadyToMerge bookmark
  case ready of
    True -> do
      changeIdToRebase <- getNextChangeId bookmark
      mergeSingleBookmark bookmark
      waitUntilBookmarkHasMerged bookmark
      mergeAllBookmarks remaining $ Just changeIdToRebase
    False -> do
      putStrLn $ "Bookmark " ++ bookmark ++ " not ready to merge yet, waiting 5 seconds."
      threadDelay $ oneSecond * 5
      mergeAllBookmarks (bookmark : remaining) Nothing

pullAndRebaseStack :: String -> IO ()
pullAndRebaseStack changeIdToRebase = callCommand "jj git fetch" >>= (\_ -> callCommand $ "jj rebase -s " ++ changeIdToRebase ++ " -d master")

waitUntilBookmarkHasMerged :: String -> IO ()
waitUntilBookmarkHasMerged bookmark = do
  hasMerged <- hasBookmarkMerged bookmark
  case hasMerged of
    True -> return ()
    False -> do
      threadDelay $ oneSecond * 5
      waitUntilBookmarkHasMerged bookmark

hasBookmarkMerged :: String -> IO Bool
hasBookmarkMerged bookmark = do
  let process = shell $ "gh pr view " ++ bookmark ++ " --json 'mergeCommit' --jq '.mergeCommit.oid'"
  hasMergeCommit <- readCreateProcess process "" >>= (\x -> pure $ unpack $ strip $ pack x) >>= (\x -> pure $ x /= "")
  return hasMergeCommit

mergeSingleBookmark :: String -> IO ()
mergeSingleBookmark bookmark = do
  callCommand $ "gh pr merge " ++ bookmark

getNextChangeId :: String -> IO String
getNextChangeId bookmark = readCreateProcess (shell $ "jj log -r " ++ bookmark ++ "+ -T 'change_id' -G") ""

isBookmarkReadyToMerge :: String -> IO Bool
isBookmarkReadyToMerge bookmark = do
  isBaseMaster <- getBaseForBookmark bookmark >>= (\x -> pure $ x == "master")
  areChecksPassing <- areChecksPassingForBookmark bookmark
  return $ isBaseMaster && areChecksPassing

areChecksPassingForBookmark :: String -> IO Bool
areChecksPassingForBookmark bookmark = do
  checks <-
    readCreateProcess (shell $ "gh pr checks " ++ bookmark ++ " --json 'bucket' --jq '.[].bucket'") ""
      >>= (\x -> return $ init $ splitOn "\n" x)
  putStrLn $ "Checks for bookmark " ++ bookmark ++ ":" ++ (show checks)
  return $ all (\x -> x == "pass") checks

getBaseForBookmark :: String -> IO String
getBaseForBookmark bookmark = do
  let process = shell $ "gh pr view " ++ bookmark ++ " --json 'baseRefName' --jq '.baseRefName'"
  output <- readCreateProcess process "" >>= (\x -> pure $ unpack $ strip $ pack x)
  putStrLn $ "Found base for bookmark " ++ bookmark ++ ": " ++ output
  return output

getOrderedBookmarksForRevset :: String -> IO [String]
getOrderedBookmarksForRevset revset = do
  let process = shell $ "jj log -r '" ++ revset ++ " & bookmarks()' -T 'bookmarks ++ \",\"' -G --reversed"
  output <- readCreateProcess process ""
  return $ init $ splitOn "," output
