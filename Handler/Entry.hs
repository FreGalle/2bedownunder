module Handler.Entry where

import Import
import Helper.Entry

getBlogEntriesR :: Handler Html
getBlogEntriesR = undefined

getBlogEntryR :: EntryId -> Handler Html
getBlogEntryR entryId = do
    entryWithAuthors <- runDB $ selectEntryWithAuthors entryId
    case entryWithAuthors of
        [] -> notFound
        ((Entity _ entry,_):_) -> do
            isPublished <- isPublishedNow $ entryPosted entry
            let authors = map (entityVal . snd) entryWithAuthors
                contentMarkup = myMarkdown $ entryContent entry
            defaultLayout $(widgetFile "entry")
