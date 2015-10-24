module Handler.Admin where

import Import
import Yesod.Text.Markdown (markdownField)

getAdminR :: Handler Html
getAdminR = do
    (entryWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        [whamlet|
<form method=post enctype=#{enctype}>
    ^{entryWidget}
    <div>
        <input type=submit value=Submit>
|]

postAdminR :: Handler Html
postAdminR = do
    ((res, entryWidget), enctype) <- runFormPost entryForm
    case res of
        FormSuccess entry -> do
            entryId <- runDB $ insert entry
            setMessage $ "New Blog Post Created"
            redirect $ HomeR
        _ -> defaultLayout $ do
            [whamlet|
<form method=post enctype=#{enctype}>
    ^{entryWidget}
    <div>
        <input type=submit value=Submit>
|]

entryForm :: Form Entry
entryForm = renderDivs $ Entry
    <$> areq textField "Title" Nothing
    <*> areq markdownField "Content" Nothing
    <*> lift (liftIO getCurrentTime)
