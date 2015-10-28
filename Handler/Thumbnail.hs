module Handler.Thumbnail where

import Import
import System.Directory             (getTemporaryDirectory, removeFile)
import Control.Monad.Trans.Resource (register)
import Yesod.Core.Types             (FileInfo (fileSourceRaw))
import qualified Graphics.ThumbnailPlus as TP

toThumbnail :: MonadResource m => FileInfo -> m FileInfo
toThumbnail info = do
    let filename = unpack . fileName $ info
    tmpFile <- liftIO $ getTmpFile filename
    -- Write image file to temporary directory
    fileSource info $$ sinkFile tmpFile
    -- Register an action to clean up the temporary image file on completion
    _ <- register $ removeFile tmpFile
    -- Create thumbnails from temporary image
    thumbs <- TP.createThumbnails def tmpFile
    case thumbs of
        TP.CreatedThumbnails (thumb:_) _ ->
            return info { fileSourceRaw = sourceFile $ TP.thumbFp thumb }
        _ ->
            return info

getTmpFile :: FilePath -> IO FilePath
getTmpFile filename = liftM (</> filename) getTemporaryDirectory
