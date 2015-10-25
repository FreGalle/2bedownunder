module Handler.Images where

import Handler.Form (imageForm)

import Import

imageFilePath :: Text -> Handler FilePath
imageFilePath f = do
    App {..} <- getYesod
    let dir = appImagesDir appSettings
        filename = unpack f
    return (dir </> filename)

getBlogImagesR :: Handler Html
getBlogImagesR = do
    ((_, widget), enctype) <- runFormPost imageForm
    images <- runDB $ selectList [ImageFilename !=. ""] [Desc ImageDate]
    defaultLayout $(widgetFile "images")

postBlogImagesR :: Handler Html
postBlogImagesR = do
    ((result, widget), enctype) <- runFormPost imageForm
    case result of
        FormSuccess (file, info, date) -> do
            -- TODO: check if image already exists
            let filename = fileName file
            path <- imageFilePath filename
            -- Save image file
            liftIO  $ fileMove file path
            _ <- runDB $ insert (Image filename info date)
            setMessage "Image saved"
            redirect BlogImagesR
        FormMissing -> do
            setMessage "Incomplete input"
            redirect BlogImagesR
        _ -> do
            setMessage "Could not save file"
            redirect BlogImagesR
