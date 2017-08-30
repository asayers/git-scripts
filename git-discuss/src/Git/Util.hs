{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Git.Util
    ( commitOnBranch
    , parseCommitOid
    , resolveCommit
    , resolveBranchTree
    , getCommitRange
    , showCommit
    , creationCommit
    , runGit
    , whoami
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Monoid
import Data.Tagged
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.LocalTime
import Git
import System.FilePath
import System.Process

-------------------------------------------------------------------------------
-- Helpers specific to git-discuss

commitOnBranch
    :: (MonadGit r m, MonadIO m)
    => RefName -> CommitMessage -> TreeT r m () -> m (Commit r)
commitOnBranch branch message mkTree = do
    author <- makeSignature
    let committer = author{ signatureName = "git-discuss", signatureEmail = "" }
    tree <- createTree mkTree
    parents <- maybeToList <$> resolveCommit branch
    createCommit parents tree author committer message (Just branch)

-------------------------------------------------------------------------------
-- Generic git helpers

whoami :: (MonadIO m) => m (Text, Text)
whoami = do
    name  <- runGit' ["config", "--get", "user.name" ]
    email <- runGit' ["config", "--get", "user.email"]
    return (T.strip name, T.strip email)

makeSignature :: (MonadIO m) => m Signature
makeSignature = do
    signatureWhen <- liftIO getZonedTime
    (signatureName, signatureEmail) <- whoami
    return Signature{..}

resolveCommit :: (MonadGit r m) => RefName -> m (Maybe (CommitOid r))
resolveCommit branch = runMaybeT $ do
    oid <- MaybeT $ resolveReference branch
    CommitObj _ <- lift $ lookupObject oid
    return $ Tagged oid

resolveBranchTree :: (MonadGit r m) => RefName -> m (Maybe (Tree r))
resolveBranchTree branch = runMaybeT $ do
    oid <- MaybeT $ resolveReference branch
    CommitObj commit <- lift $ lookupObject oid
    lift $ lookupTree $ commitTree commit

-- | Resolve the range <base>..<head>. This includes head but doesn't
-- include base.
getCommitRange
    :: (MonadGit r m, MonadIO m)
    => FilePath -> CommitOid r -> CommitOid r -> m [CommitOid r]
getCommitRange repo baseOid headOid =
    mapM (fmap Tagged . parseOid) . reverse . T.lines =<< runGit repo
        ["log"
        , "--pretty=format:%H"
        , renderObjOid baseOid <> ".." <> renderObjOid headOid
        ]

-- getPatch
--     :: (MonadGit r m, MonadIO m)
--     => FilePath -> CommitOid r -> m Patch
-- getPatch repo commit = do
--     pFullLog <- fullLog repo commit
--     let (header, _:rest) = break T.null $ T.lines pFullLog
--     let Just pSha = msum $ map (T.stripPrefix "commit ") header
--     let Just pAuthor = msum $ map (T.stripPrefix "Author: ") header
--     let pTitle = T.strip $ head rest
--     return Patch{..}

showCommit
    :: (MonadGit r m, MonadIO m)
    => FilePath -> CommitOid r -> m Text
showCommit repo commit = runGit repo ["show", renderObjOid commit]

parseCommitOid :: (MonadGit r m) => Text -> m (CommitOid r)
parseCommitOid = fmap Tagged . parseOid

-- | Find the commit which introduced a given file
creationCommit
    :: (MonadGit r m, MonadIO m)
    => FilePath -> RefName -> TreeFilePath -> m (CommitOid r)
creationCommit repo branch path =
    parseCommitOid =<< runGit repo
        [ "log"
        , "--pretty=format:%H"
        , "--diff-filter=A"
        , branch
        , "--", T.decodeUtf8 path
        ]

runGit :: (MonadIO m) => FilePath -> [Text] -> m Text
runGit repo args = liftIO $ do
    let gitDir = T.pack $ repo </> ".git"
    (_, stdout, _) <- readProcessWithExitCode "/usr/bin/git"
        (map T.unpack $ "--git-dir=" <> gitDir : args) ""
    return $ T.pack stdout

runGit' :: (MonadIO m) => [Text] -> m Text
runGit' args = liftIO $ do
    (_, stdout, _) <- readProcessWithExitCode "/usr/bin/git" (map T.unpack args) ""
    return $ T.pack stdout
