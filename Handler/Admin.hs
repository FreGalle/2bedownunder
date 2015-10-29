module Handler.Admin where

import Import
import Helper.Admin


getAdminR :: Handler Html
getAdminR = do
    (entryWidget, enctype) <- generateFormPost $ entryForm Nothing
    defaultLayout $(widgetFile "admin")

postAdminR :: Handler Html
postAdminR = do
    ((res, entryWidget), enctype) <- runFormPost $ entryForm Nothing
    case res of
        FormSuccess entry -> do
            entryId <- runDB $ insert entry
            redirect $ EntryR entryId
        _ -> defaultLayout $(widgetFile "admin")
