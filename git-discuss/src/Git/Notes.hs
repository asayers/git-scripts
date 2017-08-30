{-# LANGUAGE OverloadedStrings #-}

module Git.Notes
    ( getNotes
    , addNote
    , addTag
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Git
import Git.Util

getNotes
    :: (MonadGit r m, MonadIO m)
    => FilePath -> CommitOid r -> m Text
getNotes repo commit = removeBlank <$> runGit repo
    [ "notes"
    , "show", renderObjOid commit
    ]
  where
    removeBlank = T.unlines . filter (not . T.null) . T.lines

-- | Eg. `addTag "Reviewed"` will add the following note:
--
--    Reviewed-by: Alex Sayers <alex.sayers@gmail.com>
addTag
    :: (MonadGit r m, MonadIO m)
    => FilePath -> CommitOid r -> Text -> m ()
addTag repo commit = addNote repo commit <=< taggingNote

-- | The tag should be a capitalised verb in the past tense, eg.
-- "Reviewed", "Acked", etc.
-- FIXME: Handle case where either name or email isn't set
taggingNote :: (MonadIO m) => Text -> m Text
taggingNote tag = do
    (name, email) <- whoami
    return $ tag <> "-by: " <> name <> " <" <> email <> ">"

addNote
    :: (MonadGit r m, MonadIO m)
    => FilePath -> CommitOid r -> Text -> m ()
addNote repo commit note = void $ runGit repo
    [ "notes", "append"
    , "--message", note
    , renderObjOid commit
    ]
