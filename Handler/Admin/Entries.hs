module Handler.Admin.Entries where

import Import
import Helper.Admin
import Helper.Entry

getAdminEntriesR :: Handler Html
getAdminEntriesR = do
    now <- liftIO getCurrentTime
    published   <- runDB $ selectList [EntryPosted <=. Just now] [Desc EntryPosted]
    unpublished <- runDB $ selectList ([EntryPosted >=. Just now] ||. [EntryPosted ==. Nothing]) [Desc EntryPosted, Desc EntryCreated]
    adminLayout $(widgetFile "admin-entries")

getAdminNewEntryR :: Handler Html
getAdminNewEntryR = do
    let actionsWidget = newEntryActions
    thumbsDir <- appThumbsDir <$> getsYesod appSettings
    images <- runDB $ selectList [] [Desc ImageUploaded]

    (entryWidget, enctype) <- generateFormPost newEntryForm
    adminLayout $(widgetFile "admin-entry")

postAdminNewEntryR :: Handler Html
postAdminNewEntryR = do
    ((res, entryWidget), enctype) <- runFormPostNoToken newEntryForm
    liftIO $ print res
    case res of
        FormSuccess (entry, authorIds) -> do
            entryId <- runDB $ do
                eId <- insert entry
                _ <- forM authorIds $ \authorId -> insert (Author authorId eId)
                return eId

            published <- isPublishedNow $ entryPosted entry
            if published
                then do
                    msg <- withUrlRenderer [hamlet|Back to <a href=@{AdminEntriesR}>admin overview</a>|]
                    setMessage msg
                    redirect $ BlogEntryR entryId
                -- Go to admin overview
                else do
                    setMessage "Post created but not yet published"
                    redirect AdminEntriesR
        _ -> do
            let actionsWidget = newEntryActions
            thumbsDir <- appThumbsDir <$> getsYesod appSettings
            images <- runDB $ selectList [] [Desc ImageUploaded]
            adminLayout $(widgetFile "admin-entry")

getAdminEntryR :: EntryId -> Handler Html
getAdminEntryR entryId = do
    entryWithAuthorIds <- runDB $ selectEntryWithAuthorIds entryId
    case entryWithAuthorIds of
        [] -> notFound
        ((Entity _ entry,_):_) -> do
            let authorIds = map snd entryWithAuthorIds
            images <- runDB $ selectList [] [Desc ImageUploaded]
            thumbsDir <- appThumbsDir <$> getsYesod appSettings
            (entryWidget, enctype) <- generateFormPost $ entryForm (Just entry) authorIds
            let actionsWidget = updateEntryActions entryId
            adminLayout $(widgetFile "admin-entry")

putAdminEntryR :: EntryId -> Handler ()
putAdminEntryR entryId = do
    entry <- runDB $ get404 entryId
    ((res, _), _) <- runFormPostNoToken newEntryForm
    case res of
        FormSuccess (updatedEntry, authorIds) -> do
            setMessage $ "Post updated successfully"
            _ <- runDB $ do
                replace entryId updatedEntry
                deleteWhere [AuthorEntryId ==. entryId]
                forM authorIds $ \authorId -> insert (Author authorId entryId)
            return ()
        FormFailure fails -> do
            setMessage $ toHtml $ intercalate ", " fails
            return ()
        _ -> return ()


deleteAdminEntryR :: EntryId -> Handler ()
deleteAdminEntryR entryId = do
    runDB $ delete entryId
    setMessage "Post has been deleted"
