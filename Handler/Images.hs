module Handler.Images
    ( getBlogImagesR
    , postBlogImagesR
    , putBlogImageR
    , deleteBlogImageR
    ) where

import Import
import Helper.Images

import System.Directory (removeFile)

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
    ids <- forM files saveImg
    let ids' = catMaybes ids
    setMessage $ toHtml $ "new images saved: " ++ show (length ids')
    redirect BlogImagesR

putBlogImageR :: ImageId -> Handler ()
putBlogImageR imageId = do
    ImageUpdate {..} <- requireJsonBody :: Handler ImageUpdate
    runDB $ update imageId [ImageDescription =. description, ImageVisible =. visible]
    setMessage "Image successfully updated"

-- DELETE Handler for one image at a time
deleteBlogImageR :: ImageId -> Handler ()
deleteBlogImageR imageId = do
    image <- runDB $ get404 imageId
    (imagePath, thumbPath) <- (,) <$> imageFp image <*> thumbFp image
    liftIO $ do
        removeFile imagePath
        removeFile thumbPath
    runDB $ delete imageId
    setMessage "Image has been deleted"

-- Name used to upload image files as in the form
imagesName :: Text
imagesName = "images"
