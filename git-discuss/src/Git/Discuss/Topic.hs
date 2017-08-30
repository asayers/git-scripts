
module Git.Discuss.Topic
    ( TopicName(..)
    , Topic
    , AnnotatedTopic
    , Comment(..)
    , Metadata(..)
    , orderComments
    , addComment
    , editComment
    , deleteComment
    ) where

import Control.Monad
import Data.Function
import qualified Data.HashMap.Strict as HMS
import Data.HashTree
import Data.Text (Text)
import Data.Time.LocalTime
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID

newtype TopicName = TopicName { unTopic :: Text } deriving (Eq, Show)

type Topic = HashTree UUID Comment

data Comment = Comment
    { cBody :: Text
    } deriving (Eq, Show)

type AnnotatedTopic = HashTree UUID (Comment, Metadata)

data Metadata = Metadata
    { mAuthorName :: Text
    , mAuthorEmail :: Text
    , mTimestamp :: ZonedTime
    } deriving (Show)

-------------------------------------------------------------------------------

orderComments :: (Comment, Metadata) -> (Comment, Metadata) -> Ordering
orderComments = compare `on` zonedTimeToUTC . mTimestamp . snd

-------------------------------------------------------------------------------
-- Modifying the comment tree

addComment :: Maybe UUID -> Text -> Topic -> IO Topic
addComment parent body (HashTree cs) = do
    unless (maybe True (flip HMS.member cs) parent) $ fail "parent not found"
    uuid <- UUID.nextRandom
    let c = Comment{ cBody = body }
    return $ HashTree $ HMS.insert uuid (parent, c) cs

editComment :: UUID -> Text -> Topic -> IO Topic
editComment uuid newBody (HashTree cs) = do
    unless (HMS.member uuid cs) $ fail "target not found"
    return $ HashTree $
        HMS.adjust (\(p, c) -> (p, c{ cBody = newBody })) uuid cs

deleteComment :: UUID -> Topic -> IO Topic
deleteComment uuid (HashTree cs) = do
    unless (HMS.member uuid cs) $ fail "target not found"
    return $ HashTree $ HMS.delete uuid cs
