{-# LANGUAGE RecordWildCards #-}

-- | A shortlog is the result of running `git shortlog <range>`, and it
-- typically looks something like this:
--
--   Alex Sayers (18):
--         Benchmarks.JournalLogger: Refactor
--         T.O.ManualStrategy: improve output of `journal` manorder command
--         Test.GenerateJournalData: Take uri as argument
--         J.Backend: Remove the generic handle backend
--         J.Backend: Parse journal uris with attoparsec
--         J.Backend: Add unit test for the journal uri parser
--         J.Backend: Make the backend aware of different kinds of messages
--         Kaskakafka.Extras: Add some kafka helpers
--         J.Backend: Add a kafka backend (initial implementation)
--         Benchmarks.JournalLogger: Benchmark kafka backend
--         J.Serialise: Log file header as EntryBatch
--         J.Serialise: Don't write write empty payloads to backend
--         J.Serialise: Only one definition per payload
--         Journal: Add support for generations
--         OrderGen: Log generations as such
--         J.Backend: Batch messages by generation when logging to kafka
--         J.Backend: Log the last chunk before terminating
--         Haskakafka.Extras: Optimise Builder execution
--
-- We want to display a shortlog in the series interface; moreover, we want
-- to allow the user to click on any of the commit messages and see the
-- full commit.
module Git.Shortlog
    ( Shortlog(..)
    , CommitGroup(..)
    , shortlog
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.List
import Data.Text (Text)
import Git
import Git.Util

data Shortlog r = Shortlog
    { slCommitGroups :: [CommitGroup r]
    , slFullCommits :: [(CommitOid r, Text)]  -- In commit order
    }

data CommitGroup r = CommitGroup
    { slAuthorName :: Text
    , slCommits :: [Commit r]
    }

shortlog :: (MonadGit r m, MonadIO m) => FilePath -> CommitOid r -> CommitOid r -> m (Shortlog r)
shortlog repo baseOid headOid = do
    oids <- getCommitRange repo baseOid headOid
    commits <- forM oids $ \oid -> lookupCommit oid
    let slCommitGroups = groupShortLog commits
    slFullCommits <- mapM (\oid -> (,) oid <$> showCommit repo oid) oids
    return Shortlog{..}

groupShortLog
    :: [Commit r]
    -> [CommitGroup r]
groupShortLog = map processGroup . groupBy ((==) `on` email)
  where
    email = signatureEmail . commitAuthor
    processGroup commitGroup = CommitGroup
        { slAuthorName = signatureName $ commitAuthor $ head commitGroup
        , slCommits = commitGroup
        }
