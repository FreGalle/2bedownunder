module Handler.Entry where

import Import
import Helper.Entry
import Helper.Locale

getBlogEntriesR :: Handler Html
getBlogEntriesR = undefined

getBlogEntryR :: EntryId -> Handler Html
getBlogEntryR entryId = do
    entryWithAuthors <- runDB $ selectEntryWithAuthors entryId
    case entryWithAuthors of
        [] -> notFound
        ((Entity _ entry,_):_) -> do
            (mPrev, mNext) <- selectPreviousNext entry
            isPublished <- isPublishedNow $ entryPosted entry
            let authors = map (entityVal . snd) entryWithAuthors
                contentMarkup = myMarkdown $ entryContent entry
            defaultLayout $(widgetFile "entry")
