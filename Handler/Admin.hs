module Handler.Admin where

import Import
import Yesod.Text.Markdown (markdownField)

getAdminR :: Handler Html
getAdminR = do
    (entryWidget, enctype) <- generateFormPost $ entryForm Nothing
    defaultLayout $(widgetFile "admin")

postAdminR :: Handler Html
postAdminR = do
    ((res, entryWidget), enctype) <- runFormPost $ entryForm Nothing
    case res of
        FormSuccess entry -> do
            entryId <- runDB $ insert entry
            redirect $ EntryR entryId
        _ -> defaultLayout $(widgetFile "admin")

entryForm :: Maybe Entry -> Form Entry
entryForm mEntry = renderDivs $ Entry
    <$> areq textField titleSettings         (entryTitle <$> mEntry)
    <*> areq checkBoxField publishedSettings (entryPublished <$> mEntry)
    <*> areq markdownField contentSettings   (entryContent <$> mEntry)
    <*> lift (liftIO getCurrentTime)

titleSettings, publishedSettings, contentSettings :: FieldSettings App
titleSettings = FieldSettings
    { fsLabel = "Title"
    , fsTooltip = Nothing
    , fsId = Just "title"
    , fsName = Just "title"
    , fsAttrs = []
    }
publishedSettings = FieldSettings
    { fsLabel = "Published"
    , fsTooltip = Nothing
    , fsId = Just "published"
    , fsName = Just "published"
    , fsAttrs = []
    }
contentSettings = FieldSettings
    { fsLabel = "Content"
    , fsTooltip = Nothing
    , fsId = Just "content"
    , fsName = Just "content"
    , fsAttrs = []
    }
