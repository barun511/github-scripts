module Main (main) where

import Control.Concurrent (threadDelay)
import Data.List.Split (splitOn)
import Lib
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Process (callCommand, readCreateProcess, readProcess, shell)

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
  mergeAllBookmarks bookmarks
  exitFailure
parse _ = do
  putStrLn "Too many arguments, this function only accepts one revset"
  exitFailure

mergeAllBookmarks :: [String] -> IO ()
mergeAllBookmarks [] = exitSuccess
mergeAllBookmarks (bookmark : remaining) = do
  ready <- isBookmarkReadyToMerge bookmark
  case ready of
    True -> mergeSingleBookmark bookmark >>= (\_ -> mergeAllBookmarks remaining)
    False -> do
      putStrLn $ "Bookmark " ++ bookmark ++ " not ready to merge yet, waiting 5 seconds."
      threadDelay $ oneSecond * 5
      mergeAllBookmarks $ bookmark : remaining

mergeSingleBookmark :: String -> IO ()
mergeSingleBookmark bookmark = callCommand $ "gh pr merge " ++ bookmark

isBookmarkReadyToMerge :: String -> IO Bool
isBookmarkReadyToMerge bookmark = do
  isBaseMaster <- getBaseForBookmark bookmark >>= (\x -> pure $ x == "master")
  areChecksPassing <- areChecksPassingForBookmark bookmark
  return $ isBaseMaster && areChecksPassing

areChecksPassingForBookmark :: String -> IO Bool
areChecksPassingForBookmark bookmark = do
  checks <-
    readCreateProcess (shell $ "gh pr checks " ++ bookmark ++ " --json 'bucket' --jq '.[].bucket'") ""
      >>= (\x -> return $ splitOn "\n" x)
  return $ all (\x -> x == "pass") checks

getBaseForBookmark :: String -> IO String
getBaseForBookmark bookmark = do
  let process = shell $ "gh pr view " ++ bookmark ++ " --json 'baseRefName' --jq '.baseRefName'"
  output <- readCreateProcess process ""
  return output

getOrderedBookmarksForRevset :: String -> IO [String]
getOrderedBookmarksForRevset revset = do
  let process = shell $ "jj log -r '" ++ revset ++ "' & bookmarks()' -T 'bookmarks ++ \",\"' -G --reversed"
  output <- readCreateProcess process ""
  return $ init $ splitOn "," output
