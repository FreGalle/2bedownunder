module Handler.Images where

import Handler.Form (imageForm)

import Import

uploadDirectory :: FilePath
uploadDirectory = "static" </> "images"

imageFilePath :: Text -> FilePath
imageFilePath f = uploadDirectory </> unpack f

getImagesR :: Handler Html
getImagesR = do
    ((_, widget), enctype) <- runFormPost imageForm
    images <- runDB $ selectList [ImageFilename !=. ""] [Desc ImageDate]
    defaultLayout $(widgetFile "images")

postImagesR :: Handler Html
postImagesR = do
    ((result, widget), enctype) <- runFormPost imageForm
    case result of
        FormSuccess (file, info, date) -> do
            -- TODO: check if image already exists
            let filename = fileName file
                path = imageFilePath filename
            -- Save image file
            liftIO  $ fileMove file path
            _ <- runDB $ insert (Image filename info date)
            setMessage "Image saved"
            redirect ImagesR
        _ -> do
            setMessage "Could not save file"
            redirect ImagesR
