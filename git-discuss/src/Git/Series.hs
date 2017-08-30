{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Git.Series
    ( SeriesName(..)
    , Series(..)
    , listSeries
    , getSeries
    , updateCover
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Git
import Git.Diffstat
import Git.Notes
import Git.Shortlog
import Git.Util

newtype SeriesName = SeriesName Text deriving (Eq, Show)

data Series r = Series
    { sBase :: Text
    , sHead :: Text
    , sCover :: Text
    , sCommit :: Commit r
    , sShortlog :: Shortlog r
    , sDiffstat :: Diffstat
    , sNotes :: Text
    }

updateCover :: (MonadGit r m, MonadIO m) => SeriesName -> Text -> m ()
updateCover name _newCover =
    void $ commitOnBranch (seriesToBranch name) "Update cover" $ do
        return ()

seriesToBranch :: SeriesName -> RefName
seriesToBranch (SeriesName name) = "refs/heads/git-series/" <> name

listSeries :: (MonadGit r m) => m [SeriesName]
listSeries =
    mapMaybe (fmap SeriesName . T.stripPrefix "refs/heads/git-series/")
    <$> listReferences

getSeries
    :: (MonadGit r m, MonadIO m) => FilePath -> SeriesName -> m (Maybe (Series r))
getSeries repo name = runMaybeT $ do
    sOid <- MaybeT $ resolveReference (seriesToBranch name)
    CommitObj sCommit <- lift $ lookupObject sOid
    tree <- lift $ lookupTree $ commitTree sCommit
    entries <- lift $ listTreeEntries tree
    let findPath x = snd <$> MaybeT (return $ find (\(p,_) -> p == x) entries)
    CommitEntry baseOid            <- findPath "base"
    CommitEntry headOid            <- findPath "series"
    BlobEntry coverOid PlainBlob <- findPath "cover"
    Blob _ (BlobString sCoverBS) <- lift $ lookupBlob coverOid
    let sCover = T.decodeUtf8 sCoverBS
    let sHead = renderObjOid headOid
    let sBase = renderObjOid baseOid
    sDiffstat <- lift $ diffstat repo baseOid headOid
    sShortlog <- lift $ shortlog repo baseOid headOid
    sNotes <- lift $ getNotes repo (commitOid sCommit)
    return $ Series{..}
