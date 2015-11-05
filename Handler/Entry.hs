module Handler.Entry where

import Import

import Helper.Entry

getBlogEntriesR :: Handler Html
getBlogEntriesR = undefined

getBlogEntryR :: EntryId -> Handler Html
getBlogEntryR entryId = do
    entry <- runDB $ get404 entryId
    let mdMarkup = myMarkdown $ entryContent entry
    defaultLayout $ do
        [whamlet|
<h1>#{entryTitle entry}
<div>#{mdMarkup}
|]
