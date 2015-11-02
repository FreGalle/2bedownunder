module Helper.Admin where

import Import

import Text.Markdown (Markdown)
import Yesod.Text.Markdown (markdownField)

newEntryForm :: Maybe Entry -> Form (Text, Bool, Markdown)
newEntryForm mEntry = renderDivs $ (,,)
    <$> areq textField titleSettings (entryTitle <$> mEntry)
    <*> areq checkBoxField publishNowSettings (fmap (isJust . entryPosted) mEntry)
    <*> areq markdownField contentSettings (entryContent <$> mEntry)

titleSettings, publishNowSettings, contentSettings :: FieldSettings App
titleSettings = FieldSettings
    { fsLabel = "Title"
    , fsTooltip = Nothing
    , fsId = Just "title"
    , fsName = Just "title"
    , fsAttrs = []
    }
publishNowSettings = FieldSettings
    { fsLabel = "Post immediately"
    , fsTooltip = Nothing
    , fsId = Just "posted"
    , fsName = Just "posted"
    , fsAttrs = []
    }
contentSettings = FieldSettings
    { fsLabel = "Content"
    , fsTooltip = Nothing
    , fsId = Just "content"
    , fsName = Just "content"
    , fsAttrs = []
    }
