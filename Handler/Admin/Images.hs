module Handler.Admin.Images where

import Import
import System.Directory (removeFile)

import Helper.Admin
import Helper.Image

-- GET Handler for main images page
getAdminImagesR :: Handler Html
getAdminImagesR = do
    images <- runDB $ selectList [] [Desc ImageUploaded]
    adminLayout $(widgetFile "admin-images")

-- POST Handler for main images page
-- Used when uploading more images
postAdminImagesR :: Handler ()
postAdminImagesR = do
    files <- lookupFiles imagesName
    ids <- forM files saveImg
    let ids' = catMaybes ids
    setMessage $ toHtml $ "new images saved: " ++ show (length ids')
    redirect AdminImagesR

putAdminImageR :: ImageId -> Handler ()
putAdminImageR imageId = do
    ImageUpdate {..} <- requireJsonBody :: Handler ImageUpdate
    runDB $ update imageId [ImageDescription =. description, ImageVisible =. visible]
    setMessage "Image successfully updated"

-- DELETE Handler for one image at a time
deleteAdminImageR :: ImageId -> Handler ()
deleteAdminImageR imageId = do
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

rowWidget :: ImageId -> Image -> Widget
rowWidget imageId image = do
    thumbsDir <- appThumbsDir <$> getsYesod appSettings
    ident <- newIdent
    $(widgetFile "admin-images-row")
