module Handler.Image where

import Import

getBlogImagesR :: Handler Html
getBlogImagesR = do
    images <- runDB $ selectList [ImageVisible ==. True] [Desc ImageUploaded]
    thumbsDir <- appThumbsDir <$> getsYesod appSettings
    defaultLayout $ do
        setTitle "2 Belgen Down Under"
        $(widgetFile "images")
