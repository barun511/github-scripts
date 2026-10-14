{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JJLib
  ( getBaseForBookmark,
    maybeGetBookmarkForRevision,
    readPullRequest,
    setBookmarkAtCurrentChangeId,
    PullRequest,
    body,
    url,
  )
where

import Data.Aeson (FromJSON (parseJSON), decodeStrictText, withObject, (.:))
import Data.Text (pack, strip, unpack)
import System.Process (callCommand, readCreateProcess, shell)

getBaseForBookmark :: String -> IO String
getBaseForBookmark bookmark = do
  let process = shell $ "gh pr view " ++ bookmark ++ " --json 'baseRefName' --jq '.baseRefName'"
  output <- readCreateProcess process "" >>= (\x -> pure $ unpack $ strip $ pack x)
  putStrLn $ "Found base for bookmark " ++ bookmark ++ ": " ++ output
  return output

setBookmarkAtCurrentChangeId :: String -> IO ()
setBookmarkAtCurrentChangeId bookmark = callCommand $ "jj bookmark set " ++ bookmark

maybeGetBookmarkForRevision :: String -> IO (Maybe String)
maybeGetBookmarkForRevision revision = do
  let process = shell $ "jj log -r " ++ revision ++ " -T bookmarks -G"
  output <- readCreateProcess process ""
  pure $ if output == "" then Nothing else Just output

data PullRequest = PullRequest
  { body :: String,
    url :: String
  }
  deriving (Show, Read)

instance FromJSON PullRequest where
  parseJSON = withObject "PullRequest" $ \o -> do
    prBody <- o .: "body"
    prUrl <- o .: "url"
    return PullRequest {body = prBody, url = prUrl}

readPullRequest :: String -> IO (Maybe PullRequest)
readPullRequest bookmark = do
  putStrLn bookmark
  pullRequestJson <- readCreateProcess (shell $ "gh pr view " ++ bookmark ++ " --json 'body,url' ") ""
  putStrLn pullRequestJson
  let maybePullRequest = decodeStrictText $ pack pullRequestJson :: Maybe PullRequest
  pure $ maybePullRequest
