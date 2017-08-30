{-# LANGUAGE OverloadedStrings #-}

module Git.Review.Routes
    ( webApp
    ) where

import qualified Clay as Clay
import Control.Monad.Reader
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as UUID
import Git
import Git.Discuss.Backend
import Git.Discuss.Topic
import Git.Notes
import Git.Review.Stylesheet
import Git.Review.Views
import Git.Series
import Git.Util
import Snap.Core
import Text.Blaze.Html.Renderer.Utf8

webApp :: (MonadGit r m, MonadSnap m) => FilePath -> m ()
webApp repo = route
    [ ("/", method GET rootHandler)
    , ("/", method POST goToNewTopicHandler)
    , ("/style.css", cssHandler)
    , ("/:topic", method GET $ getTopicHandler repo)
    , ("/:topic", method POST $ postTopicHandler repo)
    ]

cssHandler :: (MonadSnap m) => m ()
cssHandler = writeText $ T.unlines
    [ TL.toStrict $ Clay.render stylesheet
    -- , T.pack $ styleToCss tango         -- TODO: add this
    ]

rootHandler :: (MonadGit r m, MonadSnap m) => m ()
rootHandler = do
    series <- listSeries
    topics <- listTopics
    writeBuilder $ renderHtmlBuilder $ renderListTopicsPage series topics

goToNewTopicHandler :: (MonadGit r m, MonadSnap m) => m ()
goToNewTopicHandler = do
    Just [topicStr] <- rqParam "topic" <$> getRequest
    redirect $ "/" <> topicStr

getTopicHandler :: (MonadGit r m, MonadSnap m) => FilePath -> m ()
getTopicHandler repo = do
    Just [topicStr] <- rqParam "topic" <$> getRequest
    let topic = TopicName $ T.decodeUtf8 topicStr
    comments <- annotate repo topic =<< readComments topic
    series <- getSeries repo (SeriesName $ unTopic topic)
    writeBuilder $ renderHtmlBuilder $ renderTopic series topic comments

postTopicHandler :: (MonadGit r m, MonadSnap m) => FilePath -> m ()
postTopicHandler repo = msum
    [replyHandler, editHandler, deleteHandler, noteHandler repo]

noteHandler :: (MonadGit r m, MonadSnap m) => FilePath -> m ()
noteHandler repo = do
    Just [topicStr] <- rqParam "topic" <$> getRequest
    Just [targetStr] <- rqParam "note-target" <$> getRequest
    Just [note] <- rqParam "note-text" <$> getRequest
    target <- parseCommitOid $ T.decodeUtf8 targetStr
    addTag repo target $ T.decodeUtf8 note
    redirect $ "/" <> topicStr

replyHandler :: (MonadGit r m, MonadSnap m) => m ()
replyHandler = do
    Just [topicStr] <- rqParam "topic" <$> getRequest
    Just [targetStr] <- rqParam "reply-target" <$> getRequest
    Just [comment] <- rqParam "reply-text" <$> getRequest
    let topic = TopicName $ T.decodeUtf8 topicStr
    comments <- readComments topic
    let target = UUID.fromASCIIBytes targetStr
    comments' <- liftIO $ addComment target (T.decodeUtf8 comment) comments
    writeComments topic comments'
    redirect $ "/" <> topicStr

editHandler :: (MonadGit r m, MonadSnap m) => m ()
editHandler = do
    Just [topicStr] <- rqParam "topic" <$> getRequest
    Just [targetStr] <- rqParam "edit-target" <$> getRequest
    Just [comment] <- rqParam "edit-text" <$> getRequest
    let topic = TopicName $ T.decodeUtf8 topicStr
    comments <- readComments topic
    target <- maybe (fail "target not a uuid") return $
        UUID.fromASCIIBytes targetStr
    comments' <- liftIO $ editComment target (T.decodeUtf8 comment) comments
    writeComments topic comments'
    redirect $ "/" <> topicStr

deleteHandler :: (MonadGit r m, MonadSnap m) => m ()
deleteHandler = do
    Just [topicStr] <- rqParam "topic" <$> getRequest
    Just [targetStr] <- rqParam "delete-target" <$> getRequest
    let topic = TopicName $ T.decodeUtf8 topicStr
    comments <- readComments topic
    target <- maybe (fail "target not a uuid") return $
        UUID.fromASCIIBytes targetStr
    comments' <- liftIO $ deleteComment target comments
    writeComments topic comments'
    redirect $ "/" <> topicStr
