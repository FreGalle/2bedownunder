module Helper.Image
    ( saveImg
    , imageFp
    , thumbFp ) where

import Import

import System.FilePath              (splitExtensions, takeFileName)
import System.Directory             (doesFileExist, getTemporaryDirectory, copyFile, removeFile)
import Control.Monad.Trans.Resource (register)
import qualified Graphics.ThumbnailPlus as TP

saveImg :: FileInfo -> Handler (Maybe (Key Image))
saveImg info = do
    (imagePath, thumbPath) <- imageThumbFp $ unpack $ fileName info
    exists <- liftIO $ doesFileExist imagePath
    if exists
        then return Nothing
        else do
            -- Create temporary thumbnail file
            tmpThumbs <- tmpThumbFps info
            --tmpThumb <- tmpThumbFp info
            liftIO $
                case tmpThumbs of
                    (tmpResized:tmpThumb:_) -> do
                        copyFile tmpResized imagePath
                        copyFile tmpThumb thumbPath
                    [] -> do
                        fileMove info imagePath
                        fileMove info thumbPath

            uploaded <- liftIO getCurrentTime
            let file  = pack $ takeFileName imagePath
                thumb = pack $ takeFileName thumbPath
            imgId <- runDB $ insert (Image file thumb Nothing uploaded True)
            return (Just imgId)

tmpThumbFps :: MonadResource m => FileInfo -> m [FilePath]
tmpThumbFps info = do
    tmpFile <- liftIO $ getTmpFile $ unpack $ fileName info
    -- Write image file to temporary directory
    fileSource info $$ sinkFile tmpFile
    -- Register an action to clean up the temporary image file on completion
    _ <- register $ removeFile tmpFile
    -- Create thumbnails from temporary image
    thumbs <- TP.createThumbnails thumbConfig tmpFile
    return $ case thumbs of
        TP.CreatedThumbnails ts _ ->
            map TP.thumbFp ts
        _ ->
            repeat tmpFile

thumbConfig :: TP.Configuration
thumbConfig = def
    { TP.maxFileSize = 30 * 1024 * 1024
    , TP.maxImageSize = TP.Size 10000 10000
    , TP.reencodeOriginal = TP.Never
    , TP.thumbnailSizes = [(TP.Size 1600 1600, Nothing), (TP.Size 512 512, Nothing)] }

getTmpFile :: FilePath -> IO FilePath
getTmpFile filename = liftM (</> filename) getTemporaryDirectory

imageFp :: Image -> Handler FilePath
imageFp img = fst <$> paths
    where paths = imageThumbFp $ unpack $ imageFilename img

thumbFp :: Image -> Handler FilePath
thumbFp img = snd <$> paths
    where paths = imageThumbFp $ unpack $ imageFilename img

imageThumbFp :: FilePath -> Handler (FilePath,FilePath)
imageThumbFp filename = do
    AppSettings {..} <- getsYesod appSettings
    let
        iFp = appImagesDir </> imageFn filename
        tFp = appImagesDir </> appThumbsDir </> thumbFn filename
    return (iFp, tFp)

imageFn :: FilePath -> FilePath
imageFn = id

thumbFn :: FilePath -> FilePath
thumbFn imgFp = name ++ "_thumb" <.> ext
    where (name,ext) = splitExtensions imgFp
