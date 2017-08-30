{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Git.Review.Views
    ( renderTopic
    , renderListTopicsPage
    ) where

import Control.Monad
import Data.Default.Class
import Data.HashTree
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Git
import Git.Diffstat
import Git.Discuss.Topic
import Git.Series
import Git.Shortlog
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Highlighting.Kate
import Text.Markdown

renderMarkdown :: Text -> H.Html
renderMarkdown = H.preEscapedToHtml . markdown settings . TL.fromStrict
  where
    settings = def
      { msBlockCodeRenderer = codeBlockRenderer
      , msXssProtect = False
      }

codeBlockRenderer :: Maybe Text -> (Text, a) -> H.Html
codeBlockRenderer lang (src,_) =
    formatHtmlBlock defaultFormatOpts $
        highlightAs (maybe "text" T.unpack lang) $ T.unpack src

scaffold :: Text -> H.Html -> H.Html
scaffold title body = H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toMarkup title
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/style.css"
    H.body $ H.div ! A.class_ "container" $ body

renderListTopicsPage :: [SeriesName] -> [TopicName] -> H.Html
renderListTopicsPage series topics = scaffold "git-discuss" $ do
    H.p "The following series exist:"
    H.ul $ mconcat $ flip map series $ \(SeriesName t) ->
        H.li $ H.a ! A.href (H.toValue $ "/" <> t) $ H.toMarkup t
    H.p "The following topics exist:"
    H.ul $ mconcat $ flip map topics $ \(TopicName t) ->
        H.li $ H.a ! A.href (H.toValue $ "/" <> t) $ H.toMarkup t
    H.p "Or make a new topic:"
    H.form ! A.method "POST" $ do
        H.input ! A.type_ "text" ! A.name "topic" ! A.value ""
        H.input ! A.class_ "create" ! A.type_ "submit" ! A.value "Create topic"

renderTopic :: (IsOid (Oid r)) => Maybe (Series r) -> TopicName -> AnnotatedTopic -> H.Html
renderTopic series topic cs = scaffold (unTopic topic) $ do
    H.div ! A.class_ "topic-title" $ do
        H.a ! A.href "/" $ "Back to listing"
        H.h1 $ H.toMarkup $ unTopic topic
    maybe (return ()) renderSeries series
    renderComments cs
    newRootForm

renderSeries :: (IsOid (Oid r)) => Series r -> H.Html
renderSeries Series{..} = H.div ! A.class_ "series" $ do
    H.div ! A.class_ "header" $ H.pre $ H.toMarkup $
        renderSigniture $ commitAuthor sCommit
    H.div ! A.class_ "cover" $ H.pre $ H.toMarkup sCover
    H.div ! A.class_ "notes" $ renderNotes (commitOid sCommit) sNotes
    H.div ! A.class_ "shortlog" $ renderShortlog sShortlog
    H.div ! A.class_ "diffstat" $ renderDiffstat sDiffstat
    H.div ! A.class_ "diffs" $ renderDiffs sShortlog sDiffstat

renderSigniture :: Signature -> Text
renderSigniture Signature{..} =
    "Author: " <> signatureName <> " <" <> signatureEmail <> ">\nDate:   " <> T.pack (show signatureWhen)

renderNotes :: (IsOid (Oid r)) => CommitOid r -> Text -> H.Html
renderNotes oid notes = do
    H.pre $ H.toMarkup notes
    H.span $
        "Tag series as " <>
        foo "reviewed" "Reviewed" <> " " <>
        foo "tested" "Tested" <> " " <>
        foo "acked" "Acked"
  where
    foo :: Text -> Text -> H.Html
    foo name note = H.span $ do
        toggleController "notes" name
        toggleLabel "notes" name "link start-shown" (H.toMarkup name)
        H.span ! A.class_ "start-hidden" $ renderNoteButton note
    renderNoteButton note = H.form ! A.method "POST" $ do
        hiddenInput "note-target" (renderObjOid oid)
        hiddenInput "note-text" note
        H.input ! A.type_ "submit" ! A.value "Tag"

hiddenInput :: H.ToValue a => H.AttributeValue -> a -> H.Html
hiddenInput name val =
    H.input ! A.type_ "hidden" ! A.name name ! A.value (H.toValue val)

renderShortlog :: (IsOid (Oid r)) => Shortlog r -> H.Html
renderShortlog Shortlog{..} =
    forM_ slCommitGroups $ \CommitGroup{..} -> do
        H.div $ H.toMarkup $
            slAuthorName <> " (" <> T.pack (show (length slCommits)) <> "):"
        H.div ! A.class_ "commit-group" $ forM_ slCommits $ \c -> H.div $ do
            H.label
                ! A.class_ "pre-link toggle-label-commit"
                ! A.for (H.toValue $ "toggle-commit-" <> renderObjOid (commitOid c))
                $ H.toMarkup (fromMaybe "" $ listToMaybe $ T.lines $ commitLog c)

-- TODO: Parse the diff here, don't use highlighting-kate
renderDiff :: Text -> H.Html
renderDiff =
    H.toHtml . formatHtmlBlock defaultFormatOpts . highlightAs "diff" . T.unpack

renderDiffstat :: Diffstat -> H.Html
renderDiffstat ds = do
    H.table ! A.class_ "diffstat" $ mapM_ renderEntry $ dsEntries ds
    H.div $ H.toMarkup $ dsSummary ds
  where
    renderEntry DSEntry{..} = H.tr $ do
        H.td $ H.label
            ! A.class_ "pre-link"
            ! A.for (H.toValue $ "toggle-diffstat-" <> T.pack dsPath)
            $ H.toMarkup dsPath
        H.td $ H.toMarkup dsStats

renderDiffs :: (IsOid (Oid r)) => Shortlog r -> Diffstat -> H.Html
renderDiffs sl ds = do
    H.div $ mapM_ foo $ slFullCommits sl
    H.div $ mapM_ bar $ dsEntries ds
  where
    foo (oid, fullCommit) = H.div $ do
        toggleController (renderObjOid oid) "commit"
        H.div ! A.class_ "start-hidden diff commit" $ do
            H.label
                ! A.class_ "link toggle-label-commit"
                ! A.style "float: right;"
                ! A.for (H.toValue $ "toggle-commit-" <> renderObjOid oid)
                $ "close"
            renderDiff fullCommit
    bar DSEntry{..} = H.div $ do
        toggleController (T.pack dsPath) "diffstat"
        H.div ! A.class_ "start-hidden diff filediff" $ do
            H.label
                ! A.class_ "link"
                ! A.style "float: right;"
                ! A.for (H.toValue $ "toggle-diffstat-" <> T.pack dsPath)
                $ "close"
            renderDiff dsDiff

newRootForm :: H.Html
newRootForm =
    H.form ! A.method "POST" $ do
        hiddenInput "reply-target" ("" :: Text)
        H.textarea ! A.class_ "new-root-box" ! A.name "reply-text" $ ""
        H.input ! A.type_ "submit" ! A.value "New comment"

renderComments :: AnnotatedTopic -> H.Html
renderComments cs =
    H.div ! A.class_ "tree" $ H.ul $ mconcat $ foldTree orderComments renderNode cs
  where
    renderNode uuid (c,m) children =
        H.li $ H.div ! A.class_ "node" $ do
            toggleController (UUID.toText uuid) "collapse"
            H.div ! A.class_ "start-hidden" $ renderCollapsedComment uuid c m
            H.div ! A.class_ "start-shown" $ do
                renderComment uuid c m
                H.ul $ mconcat $ reverse children -- children in reverse time order

renderComment :: UUID -> Comment -> Metadata -> H.Html
renderComment uuid c m = H.div ! A.class_ "comment" $ do
    -- let commentId = H.toValue $ "comment-" <> UUID.toText uuid
    H.div ! A.class_ "comment-header" $ renderCommentHeader uuid m
    H.div ! A.class_ "comment-body"   $ renderCommentBody uuid c
    H.div ! A.class_ "comment-footer" $ renderCommentFooter uuid

renderCollapsedComment :: UUID -> Comment -> Metadata -> H.Html
renderCollapsedComment uuid c m = H.div ! A.class_ "comment comment-header" $ do
    H.div $ do
        toggleLabel (UUID.toText uuid) "collapse" "collapser" "▶"
        H.span ! A.class_ "author-name" $ H.toMarkup $ mAuthorName m
        H.span ! A.class_ "author-email" $ H.toMarkup $ mAuthorEmail m
    H.div $ H.toMarkup $ summariseText $ cBody c
    H.span ! A.class_ "timestamp" $ H.toMarkup $ show $ mTimestamp m
  where
    summariseText = T.take 20 . fromMaybe "" . listToMaybe . T.lines

renderCommentHeader :: UUID -> Metadata -> H.Html
renderCommentHeader uuid m = do
    H.div $ do
        toggleLabel (UUID.toText uuid) "collapse" "collapser" "◢"
        H.span ! A.class_ "author-name" $ H.toMarkup $ mAuthorName m
        H.span ! A.class_ "author-email" $ H.toMarkup $ mAuthorEmail m
    H.span ! A.class_ "uuid" $ H.toMarkup $ show $ uuid
    H.span ! A.class_ "timestamp" $ H.toMarkup $ show $ mTimestamp m

renderCommentBody :: UUID -> Comment -> H.Html
renderCommentBody uuid c = do
    toggleController (UUID.toText uuid) "edit"
    H.div ! A.class_ "start-shown" $
        renderMarkdown $ cBody c
    H.div ! A.class_ "start-hidden" $
        H.form ! A.class_ "edit-form" ! A.method "POST" $ do
            hiddenInput "edit-target" (UUID.toText uuid)
            H.textarea ! A.class_ "reply-box" ! A.name "edit-text" $
                H.toMarkup $ cBody c
            H.input ! A.class_ "edit" ! A.type_ "submit" ! A.value "Save"

renderCommentFooter :: UUID -> H.Html
renderCommentFooter uuid = do
    H.div $ do
        toggleController (UUID.toText uuid) "reply"
        toggleLabel (UUID.toText uuid) "reply" "link start-shown" "Reply"
        H.div ! A.class_ "start-hidden" $
            H.form ! A.class_ "reply-form" ! A.method "POST" $ do
                hiddenInput "reply-target" (UUID.toText uuid)
                H.textarea ! A.class_ "reply-box" ! A.name "reply-text" $ ""
                H.input ! A.class_ "reply" ! A.type_ "submit" ! A.value "Reply"
    H.div $ do
        toggleLabel (UUID.toText uuid) "edit" "link" "Edit"
        H.span $ " "
        H.span $ do
            toggleController (UUID.toText uuid) "delete"
            toggleLabel (UUID.toText uuid) "delete" "link start-shown" "Delete"
            H.form ! A.class_ "delete-form start-hidden" ! A.method "POST" $ do
                hiddenInput "delete-target" (UUID.toText uuid)
                H.input ! A.class_ "delete" ! A.type_ "submit"
                    ! A.value "Delete"

toggleLabel :: Text -> Text -> Text -> H.Html-> H.Html
toggleLabel uniq tag classes inner = H.label
    ! A.class_ (H.toValue $ classes <> " toggle-label-" <> tag)
    ! A.for (H.toValue $ "toggle-" <> tag <> "-" <> uniq)
    $ inner

toggleController :: Text -> Text -> H.Html
toggleController uniq tag = H.input
    ! A.type_ "checkbox"
    ! A.class_ "toggle-controller"
    ! A.id (H.toValue $ "toggle-" <> tag <> "-" <> uniq)
