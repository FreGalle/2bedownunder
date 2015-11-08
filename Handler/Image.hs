module Handler.Image where

import Import

getBlogImagesR :: Handler Html
getBlogImagesR = do
    images <- runDB $ selectList [] [Desc ImageUploaded]
    thumbsDir <- appThumbsDir <$> getsYesod appSettings
    defaultLayout $(widgetFile "images")
