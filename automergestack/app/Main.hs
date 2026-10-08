{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (AsyncException (UserInterrupt), Exception (fromException), SomeException, handle, throwIO)
import Data.List.Split (splitOn)
import Data.Text (pack, strip, unpack)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Process (callCommand, readCreateProcess, shell)

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
  mergeAllBookmarks bookmarks Nothing Nothing
  exitSuccess
parse _ = do
  putStrLn "Too many arguments, this function only accepts one revset"
  exitFailure

mergeAllBookmarks :: [String] -> Maybe String -> Maybe String -> IO ()
mergeAllBookmarks [] maybeRebaseChangeId maybeDeleteChangeId = do
  maybeHandleRebaseAndDelete maybeRebaseChangeId maybeDeleteChangeId
mergeAllBookmarks (bookmark : remaining) maybeRebaseChangeId maybeDeleteChangeId = do
  maybeHandleRebaseAndDelete maybeRebaseChangeId maybeDeleteChangeId
  ready <- isBookmarkReadyToMerge bookmark
  case ready of
    True -> do
      maybeChangeIdToRebase <- maybeGetNextChangeId bookmark
      changeIdToDelete <- getCurrentChangeId bookmark
      addToMergeQueue bookmark
      waitUntilBookmarkHasMerged bookmark
      mergeAllBookmarks remaining maybeChangeIdToRebase (Just changeIdToDelete)
    False -> do
      putStrLn $ "Bookmark " ++ bookmark ++ " not ready to merge yet, waiting 5 seconds."
      threadDelay $ oneSecond * 5
      mergeAllBookmarks (bookmark : remaining) Nothing Nothing

maybeHandleRebaseAndDelete :: Maybe String -> Maybe String -> IO ()
maybeHandleRebaseAndDelete maybeRebaseChangeId maybeDeleteChangeId = do
  case maybeRebaseChangeId of
    Just rebaseChangeId -> pullAndRebaseStack rebaseChangeId
    Nothing -> pure ()
  case maybeDeleteChangeId of
    Just changeIdToDelete -> abandonChangeId changeIdToDelete
    Nothing -> pure ()

abandonChangeId :: String -> IO ()
abandonChangeId changeIdToDelete = callCommand $ "jj abandon " ++ changeIdToDelete

pullAndRebaseStack :: String -> IO ()
pullAndRebaseStack changeIdToRebase =
  callCommand "jj git fetch"
    >>= (\_ -> callCommand $ "jj rebase -s " ++ changeIdToRebase ++ " -d master")
    >>= (\_ -> callCommand $ "jj git push -r " ++ changeIdToRebase ++ "::")

waitUntilBookmarkHasMerged :: String -> IO ()
waitUntilBookmarkHasMerged bookmark = do
  putStrLn $ "Waiting for bookmark " ++ bookmark ++ " to merge"
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

addToMergeQueue :: String -> IO ()
addToMergeQueue bookmark = do
  callCommand $ "gh pr merge " ++ bookmark

maybeGetNextChangeId :: String -> IO (Maybe String)
maybeGetNextChangeId bookmark =
  readCreateProcess (shell $ "jj log -r " ++ bookmark ++ "+ -T 'change_id' -G") ""
    >>= (\x -> pure $ if x == "" then Nothing else Just x)

getCurrentChangeId :: String -> IO String
getCurrentChangeId bookmark = readCreateProcess (shell $ "jj log -r " ++ bookmark ++ " -T 'change_id' -G") ""

isBookmarkReadyToMerge :: String -> IO Bool
isBookmarkReadyToMerge bookmark = do
  isBaseMaster <- getBaseForBookmark bookmark >>= (\x -> pure $ x == "master")
  areChecksPassing <- handle checksPassingHandler $ areChecksPassingForBookmark bookmark
  return $ isBaseMaster && areChecksPassing

getBaseForBookmark :: String -> IO String
getBaseForBookmark bookmark = do
  let process = shell $ "gh pr view " ++ bookmark ++ " --json 'baseRefName' --jq '.baseRefName'"
  output <- readCreateProcess process "" >>= (\x -> pure $ unpack $ strip $ pack x)
  putStrLn $ "Found base for bookmark " ++ bookmark ++ ": " ++ output
  return output

checksPassingHandler :: SomeException -> IO Bool
checksPassingHandler exception
  | (fromException exception) == Just UserInterrupt = throwIO exception
  | otherwise = return False

areChecksPassingForBookmark :: String -> IO Bool
areChecksPassingForBookmark bookmark = do
  checksArray <-
    readCreateProcess (shell $ "gh pr checks " ++ bookmark ++ " --json 'bucket' --jq '.[].bucket'") ""
      >>= (\x -> return $ splitOn "\n" x)
  case length checksArray of
    1 -> pure False
    _ -> do
      putStrLn $ "Checks for bookmark " ++ bookmark ++ ":" ++ (show $ init checksArray)
      return $ all (\x -> x == "pass") $ init checksArray

getOrderedBookmarksForRevset :: String -> IO [String]
getOrderedBookmarksForRevset revset = do
  let process = shell $ "jj log -r '" ++ revset ++ " & bookmarks()' -T 'bookmarks ++ \",\"' -G --reversed"
  output <- readCreateProcess process ""
  return $ init $ splitOn "," output
