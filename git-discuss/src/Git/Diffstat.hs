{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A diffstat is the result of running `git diff --stat <range>`, and it
-- typically looks something like this:
--
--   src/Benchmarks/JournalLogger.hs         | 119 ++++++++++++-------
--   src/Benchmarks/JournalSerialiser.hs     |   5 +-
--   src/Haskakafka/Extras.hs                |  72 +++++++++++
--   src/Journal/Backend.hs                  | 203 +++++++++++++++++++++++++++-----
--   src/Journal/Logger.hs                   |  18 ++-
--   src/Journal/Serialise.hs                |  76 +++++++-----
--   src/Journal/Serialise/Internal.hs       |  62 +++++-----
--   src/Journal/Test/GenerateJournalData.hs |  22 ++--
--   src/NewOrder/Broker/OMX.hs              |   5 +-
--   src/Tests/Unit.hs                       |   2 +
--   src/Trader/Order/ManualStrategy.hs      |   5 +-
--   src/Trader/Order/OrderGen.hs            |   2 +-
--   test-data/journal.decode.expected       |   8 +-
--   13 files changed, 439 insertions(+), 160 deletions(-)
--
-- We want to display a diffstat in the series interface; moreover, we want
-- to allow the user to click on any of the paths and see the full diff for
-- that file.
--
-- Warning: this module is pretty hacky - we're parsing the output of
-- porcelain commands. TODO: rewrite with libgit2.
module Git.Diffstat
    ( Diffstat(..)
    , DSEntry(..)
    , diffstat
    ) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Attoparsec.Text as A
import Data.Char
import qualified Data.HashMap.Strict as HMS
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Git
import Git.Util

data Diffstat = Diffstat
    { dsEntries :: [DSEntry]
    , dsSummary :: Text -- ^ "13 files changed, 439 insertions(+), 160 deletions(-)"
    } deriving (Eq, Show)

data DSEntry = DSEntry
    { dsPath :: FilePath  -- ^ "src/Benchmarks/JournalLogger.hs"
    , dsStats :: Text     -- ^ " | 119 ++++++++++++-------"
    , dsDiff :: Text      -- ^ Full output of `git diff <range> -- <path>`
    } deriving (Eq, Show)

diffstat
    :: (MonadIO m, MonadGit r m)
    => FilePath    -- ^ Path to repo
    -> CommitOid r -- ^ Base revision
    -> CommitOid r -- ^ Head revision
    -> m Diffstat
diffstat repo baseOid headOid = do
    parseDiffStatPatch <$> getDiffStatPatch repo baseOid headOid

-- | Returns the diffstat followed by diffs for each file
getDiffStatPatch
    :: (MonadIO m, MonadGit r m)
    => FilePath -> CommitOid r -> CommitOid r -> m Text
getDiffStatPatch repo baseOid headOid = runGit repo
    [ "diff"
    , "--stat=80"
    , "--patch"
    , renderObjOid baseOid <> ".." <> renderObjOid headOid
    ]

-- | Parses the output of `git diff --stat --patch` into a Diffstat
parseDiffStatPatch :: Text -> Diffstat
parseDiffStatPatch diffStatPatch =
    case T.splitOn "\ndiff" diffStatPatch of
        [] -> error "No files changed"
        (diffstatStr:fileDiffs) -> case reverse (T.lines diffstatStr) of
            [] -> error "No summary line"
            (summary:dslines) ->
                let diffMap = HMS.fromList $ do
                        diff <- fileDiffs
                        let diff' = "diff" <> diff <> "\n"
                        Right (_, path) <- pure $ A.parseOnly diffHeaderParser diff'
                        return (path, diff')
                    entries = do
                        dsline <- reverse dslines
                        Right (path, stats) <- pure $ A.parseOnly diffStatLineParser dsline
                        Just diff <- pure $ HMS.lookup path diffMap
                        return $ DSEntry path stats diff
                in Diffstat entries (T.strip summary)

diffStatLineParser :: A.Parser (FilePath, Text)
diffStatLineParser = do
    void $ A.string " "
    path <- A.takeTill isSpace
    A.skipSpace
    stats <- A.takeTill A.isEndOfLine
    return (T.unpack path, " " <> stats)

-- | Example diff header:
-- "diff --git a/src/Benchmarks/JournalLogger.hs b/src/Benchmarks/JournalLogger.hs"
diffHeaderParser :: A.Parser (FilePath, FilePath)
diffHeaderParser = do
    void $ A.string "diff --git a/"
    oldPath <- A.takeTill isSpace
    void $ A.string " b/"
    newPath <- A.takeTill isSpace
    A.endOfLine
    return (T.unpack oldPath, T.unpack newPath)
