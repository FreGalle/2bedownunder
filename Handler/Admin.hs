module Handler.Admin where

import Handler.Form (wrap, entryForm)

import Import

getAdminR :: Handler Html
getAdminR = do
    (entryWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ wrap entryWidget enctype


postAdminR :: Handler Html
postAdminR = do
    ((res, entryWidget), enctype) <- runFormPost entryForm
    case res of
        FormSuccess entry -> do
            entryId <- runDB $ insert entry
            setMessage "New Blog Post Created"
            redirect HomeR
        _ -> defaultLayout $ wrap entryWidget enctype
