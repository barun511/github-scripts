{-# LANGUAGE OverloadedStrings #-}

module JJLib
  ( getBaseForBookmark,
  )
where

import Data.Text (pack, strip, unpack)
import System.Process (readCreateProcess, shell)

getBaseForBookmark :: String -> IO String
getBaseForBookmark bookmark = do
  let process = shell $ "gh pr view " ++ bookmark ++ " --json 'baseRefName' --jq '.baseRefName'"
  output <- readCreateProcess process "" >>= (\x -> pure $ unpack $ strip $ pack x)
  putStrLn $ "Found base for bookmark " ++ bookmark ++ ": " ++ output
  return output
