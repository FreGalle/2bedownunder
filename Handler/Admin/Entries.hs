module Handler.Admin.Entries where

import Import
import Helper.Admin

getAdminEntriesR :: Handler Html
getAdminEntriesR = do
    now <- liftIO getCurrentTime
    published   <- runDB $ selectList [EntryPosted <=. Just now]  [Desc EntryPosted]
    unpublished <- runDB $ selectList ([EntryPosted >=. Just now] ||. [EntryPosted ==. Nothing]) [Desc EntryCreated]
    defaultLayout $(widgetFile "admin-entries")

getAdminNewEntryR :: Handler Html
getAdminNewEntryR = do
    (entryWidget, enctype) <- generateFormPost $ newEntryForm Nothing
    defaultLayout $(widgetFile "admin-entry")

postAdminNewEntryR :: Handler Html
postAdminNewEntryR = do
    ((res, entryWidget), enctype) <- runFormPost $ newEntryForm Nothing
    case res of
        FormSuccess (title, postNow, content) -> do
            now <- liftIO getCurrentTime
            let postTime   = if postNow then Just now else Nothing
                createTime = now
            entryId <- runDB $ insert $ Entry title content postTime createTime
            redirect $ BlogEntryR entryId
        _ -> defaultLayout $(widgetFile "admin-entry")

getAdminEntryR :: EntryId -> Handler Html
getAdminEntryR entryId = do
    entry <- runDB $ get404 entryId
    (entryWidget, enctype) <- generateFormPost $ newEntryForm $ Just entry
    defaultLayout $(widgetFile "admin-entry")

putAdminEntryR :: EntryId -> Handler ()
putAdminEntryR entryId = undefined

deleteAdminEntryR :: EntryId -> Handler ()
deleteAdminEntryR  entryId = undefined
