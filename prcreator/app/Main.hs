{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Text (pack, replace, unpack)
import JJLib (PullRequest (body, url), maybeGetBookmarkForRevision, readPullRequest, setBookmarkAtCurrentChangeId)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Process (callCommand)

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse [] = putStrLn "You must provide a bookmark name to build this PR" >> exitFailure
parse [bookmark] = do
  setBookmarkAtCurrentChangeId bookmark
  pushToRemote
  previousBookmark <- maybeGetBookmarkForRevision $ bookmark ++ ['-']
  createPullRequest (fromMaybe "master" previousBookmark) bookmark
  pure ()
parse _ = putStrLn "You may only provide one bookmark at a time" >> exitFailure

pushToRemote :: IO ()
pushToRemote = callCommand $ "jj git push -r @"

createPullRequest :: String -> String -> IO ()
createPullRequest previousBookmark currentBookmark = do
  pullRequest <- readPullRequest previousBookmark
  let maybeOptions = fmap makePRCreationOptions pullRequest
  let command = "gh pr create -d -f -B " ++ previousBookmark ++ " -H " ++ currentBookmark ++ fromMaybe "" maybeOptions
  callCommand command
  return ()

makePRCreationOptions :: PullRequest -> String
makePRCreationOptions pullRequest = " --body " ++ "\"" ++ (constructBodyFromPreviousPullRequest pullRequest) ++ "\""

constructBodyFromPreviousPullRequest :: PullRequest -> String
constructBodyFromPreviousPullRequest previousPullRequest = do
  let pullRequestBody = body previousPullRequest
  let pullRequestUrl = url previousPullRequest
  let newBody = replace "## PR Stack\r\n\r\n" (pack ("## PR Stack\r\n\r\n" ++ "- " ++ pullRequestUrl ++ "\r\n")) (pack pullRequestBody)
  unpack newBody
