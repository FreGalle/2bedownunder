module Handler.Admin.Entries where

import Import
import Helper.Admin

getAdminEntriesR :: Handler Html
getAdminEntriesR = undefined

getAdminNewEntryR :: Handler Html
getAdminNewEntryR = do
    (entryWidget, enctype) <- generateFormPost $ entryForm Nothing
    defaultLayout $(widgetFile "admin-new-entry")

postAdminNewEntryR :: Handler Html
postAdminNewEntryR = do
    ((res, entryWidget), enctype) <- runFormPost $ entryForm Nothing
    case res of
        FormSuccess entry -> do
            entryId <- runDB $ insert entry
            redirect $ BlogEntryR entryId
        _ -> defaultLayout $(widgetFile "admin-new-entry")

getAdminEntryR :: EntryId -> Handler Html
getAdminEntryR entryId = undefined

putAdminEntryR :: EntryId -> Handler ()
putAdminEntryR entryId = undefined

deleteAdminEntryR :: EntryId -> Handler ()
deleteAdminEntryR  entryId = undefined
