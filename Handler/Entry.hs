module Handler.Entry where

import Import

import Helper.Entry

getBlogEntriesR :: Handler Html
getBlogEntriesR = undefined

getBlogEntryR :: EntryId -> Handler Html
getBlogEntryR entryId = do
    entry <- runDB $ get404 entryId
    let contentMarkup = myMarkdown $ entryContent entry
    defaultLayout $(widgetFile "entry")
