module Handler.Entry where

import Import

getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
    entry <- runDB $ get404 entryId
    defaultLayout $ do
        [whamlet|
<h1>#{entryTitle entry}
<div>#{entryContent entry}
|]

postEntryR :: EntryId -> Handler Html
postEntryR = undefined
