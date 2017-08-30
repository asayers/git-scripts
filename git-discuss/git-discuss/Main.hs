{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.IO.Class
import Data.HashTree
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Git
import Git.Discuss.Backend
import Git.Discuss.Topic
import Git.Libgit2
import System.Environment

main :: IO ()
main = do
    let repo = "."
    withRepository lgFactory repo $ liftIO getArgs >>= \case
        "show":topic:_ -> showTopic repo (TopicName $ T.pack topic)
        "show":[] -> showTopic repo (TopicName "default")  -- TODO: use branch
        -- "list"
        -- "new"
        -- "reply"
        -- "edit"
        -- "delete"
        -- "help"
        _ -> liftIO $ putStrLn "usage: git discuss show"

showTopic :: (MonadGit r m, MonadIO m) => FilePath -> TopicName -> m ()
showTopic repo topic = do
    comments <- annotate repo topic =<< readComments topic
    liftIO $ putStr $ T.unpack $ ppComments comments

ppComments :: AnnotatedTopic -> Text
ppComments = T.unlines . concat . foldTree orderComments
    (\_uuid (c, m) children -> ppComment c m : map ("  " <>) (concat children))
  where
    ppComment :: Comment -> Metadata -> Text
    ppComment c m = mconcat
        [ "[" <> T.pack (show $ mTimestamp m) <> "] " <> mAuthorName m <> ": "
        , T.replace "\n" "" (cBody c)
        ]
