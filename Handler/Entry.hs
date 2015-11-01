module Handler.Entry where

import Import

getBlogEntriesR :: Handler Html
getBlogEntriesR = undefined

getBlogEntryR :: EntryId -> Handler Html
getBlogEntryR entryId = do
    entry <- runDB $ get404 entryId
    defaultLayout $ do
        [whamlet|
<h1>#{entryTitle entry}
<div>#{entryContent entry}
|]
