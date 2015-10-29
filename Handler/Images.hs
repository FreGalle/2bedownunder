module Handler.Images
    ( getBlogImagesR
    , postBlogImagesR
    , putBlogImageR
    , deleteBlogImageR
    ) where

import Import
import Helper.Images

import System.FilePath (splitExtensions, takeFileName)
import System.Directory (doesFileExist, removeFile)

-- GET Handler for main images page
getBlogImagesR :: Handler Html
getBlogImagesR = do
    images <- runDB $ selectList [] [Desc ImageUploaded]
    defaultLayout $(widgetFile "images")
    where
        rowWidget :: ImageId -> Image -> Widget
        rowWidget imageId image = do
            thumbsDir <- appThumbsDir <$> getsYesod appSettings
            ident <- newIdent
            $(widgetFile "row")


-- POST Handler for main images page
-- Used when uploading more images
postBlogImagesR :: Handler ()
postBlogImagesR = do
    files <- lookupFiles imagesName
    ids <- forM files saveFile
    let ids' = catMaybes ids
    setMessage $ toHtml $ "new images saved: " ++ show (length ids')

putBlogImageR :: ImageId -> Handler ()
putBlogImageR imageId = do
    ImageUpdate {..} <- requireJsonBody :: Handler ImageUpdate
    runDB $ update imageId [ImageDescription =. description, ImageVisible =. visible]
    setMessage "Image successfully updated"

-- DELETE Handler for one image at a time
deleteBlogImageR :: ImageId -> Handler ()
deleteBlogImageR imageId = do
    image <- runDB $ get404 imageId
    (filePath, thumbPath) <- getPaths $ imageFilename image
    liftIO $ do
        removeFile filePath
        removeFile thumbPath
    runDB $ delete imageId
    setMessage "Image has been deleted"

saveFile :: FileInfo -> Handler (Maybe (Key Image))
saveFile info = do
    (filePath, thumbPath) <- getPaths $ fileName info
    thumbnail <- toThumbnail info
    fileExists <- liftIO $ do
        -- Save file to disk if it doesn't exist yet
        exists <- doesFileExist filePath
        unless exists $ fileMove info filePath
        return exists

    if fileExists
        then return Nothing
        else do
            -- Save thumbnail to disk
            liftIO $ fileMove thumbnail thumbPath
            -- Persist new Image entity
            uploaded <- liftIO getCurrentTime
            let file  = fileName info
                thumb = pack $ takeFileName thumbPath
            imgId <- runDB $ insert (Image file thumb Nothing uploaded True)
            return (Just imgId)

getPaths :: Text -> Handler (FilePath, FilePath)
getPaths filename = do
    AppSettings {..} <- getsYesod appSettings
    let file   = unpack filename
        file'  = appImagesDir </> file
        thumb  = imageThumb file
        thumb' = appImagesDir </> appThumbsDir </> thumb
    return (file',thumb')

imageThumb :: FilePath -> FilePath
imageThumb file = name ++ "_thumb" <.> ext
    where (name,ext) = splitExtensions file

-- Name used to upload image files as in the form
imagesName :: Text
imagesName = "images"
