module Helper.Entry
    ( myMarkdown
    , selectEntryWithAuthors
    , selectEntryWithAuthorIds ) where

import Import

import qualified Data.Map as Map
import Text.Markdown
    ( Markdown (..)
    , MarkdownSettings (..)
    , FencedHandler
    , htmlFencedHandler, markdown
    )
import Text.Markdown.Block (Block(..))
import Text.Markdown.Inline (Inline(..))
import Text.Blaze.Html ()
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

selectEntryWithAuthors :: (MonadIO m) => EntryId -> SqlPersistT m [(Entity Entry, Entity User)]
selectEntryWithAuthors entryId =
    E.select $
    E.from $ \(entry `E.InnerJoin` entryAuthor `E.InnerJoin` user) -> do
        E.on $ user ^. UserId E.==. entryAuthor ^. AuthorUserId
        E.on $ entry ^. EntryId E.==. entryAuthor ^. AuthorEntryId
        E.where_ (entry ^. EntryId E.==. E.val entryId)
        E.orderBy [E.asc (user ^. UserName)]
        return (entry, user)

selectEntryWithAuthorIds :: (MonadIO m) => EntryId -> SqlPersistT m [(Entity Entry, UserId)]
selectEntryWithAuthorIds entryId = do
    selected <- E.select $
        E.from $ \(entry `E.InnerJoin` entryAuthor) -> do
            E.on $ entry ^. EntryId E.==. entryAuthor ^. AuthorEntryId
            E.where_ (entry ^. EntryId E.==. E.val entryId)
            return (entry, entryAuthor ^. AuthorUserId)
    return $ fmap (second E.unValue) selected

myMarkdown :: Markdown -> Html
myMarkdown (Markdown text) = markdown settings text
    where
        settings = def {
            msFencedHandlers = fences,
            msBlockFilter = noImgPs
        }
        fences = figureFenced `Map.union` columnFenced `Map.union` msFencedHandlers def
        noImgPs (pre : (BlockPara [img@InlineImage {}]:bs)) = pre : BlockPlainText [img]: noImgPs bs
        noImgPs (pre : (BlockPara [link@(InlineLink _ _ [InlineImage {}])]:bs)) = pre : BlockPlainText [link] : noImgPs bs
        noImgPs (h:hs) = h : noImgPs hs
        noImgPs [] = []

columnFenced :: Map Text (Text -> FencedHandler)
columnFenced = htmlFencedHandler
    "~~~"
    (\n -> "<div class=\"columns columns-" ++ n ++ "\">")
    (const "</div>")

figureFenced :: Map Text (Text -> FencedHandler)
figureFenced = htmlFencedHandler
    "@@@"
    (const "<figure>")
    (\description -> "<figcaption>" ++ description ++ "</figcaption></figure>")
