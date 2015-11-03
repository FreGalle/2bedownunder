module Handler.Admin.Entries where

import Import
import Helper.Admin

getAdminEntriesR :: Handler Html
getAdminEntriesR = do
    now <- liftIO getCurrentTime
    published   <- runDB $ selectList [EntryPosted <=. Just now]  [Desc EntryPosted]
    unpublished <- runDB $ selectList ([EntryPosted >=. Just now] ||. [EntryPosted ==. Nothing]) [Desc EntryPosted, Desc EntryCreated]
    adminLayout $(widgetFile "admin-entries")

getAdminNewEntryR :: Handler Html
getAdminNewEntryR = do
    (entryWidget, enctype) <- generateFormPost newEntryForm
    adminLayout $(widgetFile "admin-entry-new")

postAdminNewEntryR :: Handler Html
postAdminNewEntryR = do
    ((res, entryWidget), enctype) <- runFormPost newEntryForm
    case res of
        FormSuccess (title, postNow, content) -> do
            now <- liftIO getCurrentTime
            let postTime   = if postNow then Just now else Nothing
                createTime = now
            -- Insert the new blog post into the database
            entryId <- runDB $ insert $ Entry title content postTime createTime
            -- Go to new blog post
            if postNow
                then do
                    msg <- withUrlRenderer [hamlet|Back to <a href=@{AdminEntriesR}>admin overview</a>|]
                    setMessage msg
                    redirect $ BlogEntryR entryId
                -- Go to admin overview
                else do
                    setMessage "Post created but not yet published"
                    redirect $ AdminEntriesR

        _ -> adminLayout $(widgetFile "admin-entry-new")

getAdminEntryR :: EntryId -> Handler Html
getAdminEntryR entryId = do
    entry <- runDB $ get404 entryId
    (entryWidget, enctype) <- generateFormPost $ mUpdateEntryForm $ Just entry
    adminLayout $(widgetFile "admin-entry-update")

putAdminEntryR :: EntryId -> Handler ()
putAdminEntryR entryId = do
    entry <- runDB $ get404 entryId
    ((res, entryWidget), enctype) <- runFormPostNoToken $ mUpdateEntryForm $ Just entry
    case res of
        FormSuccess updatedEntry -> do
            setMessage $ "Post updated successfully"
            runDB $ replace entryId updatedEntry
            return ()
        FormFailure fails -> do
            setMessage $ toHtml $ intercalate ", " fails
            return ()
        _ -> return ()


deleteAdminEntryR :: EntryId -> Handler ()
deleteAdminEntryR entryId = do
    runDB $ delete entryId
    setMessage "Post has been deleted"
