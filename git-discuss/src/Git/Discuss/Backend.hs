{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Git.Discuss.Backend
    ( listTopics
    , annotate
    , writeComments
    , readComments
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HMS
import Data.HashTree
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Git
import Git.Discuss.Topic
import Git.Util

-------------------------------------------------------------------------------
-- Listing discussion branches

listTopics :: (MonadGit r m) => m [TopicName]
listTopics =
    mapMaybe (fmap TopicName . T.stripPrefix "refs/heads/git-discuss/")
    <$> listReferences

-------------------------------------------------------------------------------
-- Retrieving metadata

annotate
    :: (MonadGit r m, MonadIO m)
    => FilePath -> TopicName -> Topic -> m AnnotatedTopic
annotate repo topic cs = traverseMaybeWithKey
    (\uuid c -> fmap ((,) c) <$> getMetadata repo topic cs uuid) cs

getMetadata
    :: (MonadGit r m, MonadIO m)
    => FilePath -> TopicName -> Topic -> UUID -> m (Maybe Metadata)
getMetadata repo topic cs uuid = runMaybeT $ do
    sha <- lift $ creationCommit repo (topicToBranch topic) (uuidToPath cs uuid)
    commit <- lift $ lookupCommit sha
    return Metadata
        { mAuthorName = signatureName $ commitAuthor commit
        , mAuthorEmail = signatureEmail $ commitAuthor commit
        , mTimestamp = signatureWhen $ commitAuthor commit
        }

-------------------------------------------------------------------------------
-- Reading/writing comments

topicToBranch :: TopicName -> RefName
topicToBranch (TopicName x) = "refs/heads/git-discuss/" <> x

writeComments :: (MonadGit r m, MonadIO m) => TopicName -> Topic -> m ()
writeComments topic cs = do
    void $ commitOnBranch (topicToBranch topic) message (commentsToTree cs)
  where
    message = "Update comments"

readComments :: (MonadGit r m) => TopicName -> m Topic
readComments topic = do
    tree <- resolveBranchTree (topicToBranch topic)
    maybe (return mempty) treeToComments tree

-------------------------------------------------------------------------------
-- Representing comments as a tree

treeToComments :: (MonadGit r m) => Tree r -> m Topic
treeToComments tree = do
    entries <- listTreeEntries tree
    comments <- forM entries $ \(path, entry) -> runMaybeT $ do
        (uuid, parent) <- MaybeT $ return $ pathToUUID path
        BlobEntry oid PlainBlob <- return entry
        Blob _ (BlobString body) <- lift $ lookupBlob oid
        return (uuid, (parent, Comment{ cBody = T.decodeUtf8 body }))
    return $ HashTree $ HMS.fromList $ catMaybes comments

-- | Add missing commits to the repo and create a tree object. Idempotent.
commentsToTree :: (MonadGit r m) => Topic -> TreeT r m ()
commentsToTree cs = void $ HMS.traverseWithKey foo (nodes cs) where
    foo uuid (_, c) = do
        blob <- lift $ createBlob $ BlobString $ T.encodeUtf8 $ cBody c
        putBlob (uuidToPath cs uuid) blob

-- >>> uuidToPath cs c2cc10e1-57d6-4b6f-9899-38d972112d83
-- "c2cc10e1-57d6-4b6f-9899-38d972112d81/c2cc10e1-57d6-4b6f-9899-38d972112d83/message"
uuidToPath :: Topic -> UUID -> TreeFilePath
uuidToPath cs = BS.concat . reverse . ("message":). go where
    go c = (UUID.toASCIIBytes c <> "/") : maybe [] go (join $ lookupParent c cs)

-- >>> pathToUUID "c2cc10e1-57d6-4b6f-9899-38d972112d81/c2cc10e1-57d6-4b6f-9899-38d972112d83/message"
-- Just (c2cc10e1-57d6-4b6f-9899-38d972112d83, Just c2cc10e1-57d6-4b6f-9899-38d972112d81)
pathToUUID :: TreeFilePath -> Maybe (UUID, Maybe UUID)
pathToUUID path = case reverse (BS.split '/' path) of
    "message":uuid:parent:_ ->
        (,) <$> UUID.fromASCIIBytes uuid <*> fmap Just (UUID.fromASCIIBytes parent)
    "message":uuid:_ ->
        (,) <$> UUID.fromASCIIBytes uuid <*> pure Nothing
    _ -> Nothing

