module Handler.Images
    ( getBlogImagesR
    , postBlogImagesR
    , putBlogImageR
    , deleteBlogImageR
    ) where

import Import
import System.Directory (doesFileExist, removeFile)

-- GET Handler for main images page
getBlogImagesR :: Handler Html
getBlogImagesR = do
    let enctype = Multipart
    images <- runDB $ selectList [] [Desc ImageDate]
    defaultLayout $(widgetFile "images")

-- POST Handler for main images page
-- Used when uploading more images
postBlogImagesR :: Handler Html
postBlogImagesR = do
    files <- lookupFiles imagesName
    ids <- forM files saveFile
    let ids' = catMaybes ids
    setMessage $ toHtml $ "new images saved: " ++ show (length ids')
    --setMessage $ (pack . show $ length ids') ++ "new images saved"
    redirect BlogImagesR

putBlogImageR :: ImageId -> Handler ()
putBlogImageR imageId = undefined

deleteBlogImageR :: ImageId -> Handler ()
deleteBlogImageR imageId = do
    image <- runDB $ get404 imageId
    let filename = imageFilename image
    path <- imageFilePath filename
    -- Remove file from disk
    liftIO $ removeFile path
    -- Check if file has been successfully deleted
    stillExists <- liftIO $ doesFileExist path
    if not stillExists
        then do
            runDB $ delete imageId
            setMessage "Image has been deleted"
            redirect BlogImagesR
        else
            redirect BlogImagesR

saveFile :: FileInfo -> Handler (Maybe (Key Image))
saveFile info = do
    let filename = fileName info
    path <- imageFilePath filename
    exists <- liftIO $ doesFileExist path
    if not exists
        then do
            -- Save file to disk
            liftIO $ fileMove info path
            -- Save image to db
            day <- liftIO $ fmap utctDay getCurrentTime
            imgId <- runDB $ insert (Image filename Nothing day True)
            return (Just imgId)
        else
            return Nothing

-- Name used to upload image files as in the form
imagesName :: Text
imagesName = "images"

imageFilePath :: Text -> Handler FilePath
imageFilePath f = do
    App {..} <- getYesod
    let dir = appImagesDir appSettings
        filename = unpack f
    return (dir </> filename)
