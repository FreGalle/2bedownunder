module Helper.Admin where

import Import

import Data.Time.LocalTime
import Text.Hamlet (hamletFile)
import Text.Julius (juliusFile)
import Text.Lucius (luciusFile)
import Yesod.Text.Markdown (markdownField)

adminLayout :: Widget -> Handler Html
adminLayout widget = do
    mmsg <- getMessage
    maid <- maybeAuthId
    pc <- widgetToPageContent $ do
        toWidget $(luciusFile "templates/default-layout-wrapper.lucius")
        $(widgetFile "admin-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

newEntryForm :: Html -> MForm Handler (FormResult (Entry, [UserId]), Widget)
newEntryForm = entryForm Nothing []

newEntryActions :: Widget
newEntryActions = do
    toWidget
        [hamlet|<input #submit type=submit value=Submit>|]

updateEntryActions :: EntryId -> Widget
updateEntryActions entryId = do
    toWidget
        [hamlet|
            <button #update type=button>Update
            <button #delete type=button>Delete
        |]
    toWidget
        $(juliusFile "templates/admin-entry-form-update.julius")

entryForm :: Maybe Entry -> [UserId] -> Html -> MForm Handler (FormResult (Entry, [UserId]), Widget)
entryForm mEntry authors _ = do
    let authorOpts = optionsPersistKey [] [Asc UserName] userName
    (titleRes, titleView)     <- mreq textField titleSettings (entryTitle <$> mEntry)
    (dayRes, dayView)         <- mopt dayField dateSettings (entryDate <$> mEntry)
    (timeRes, timeView)       <- mopt timeFieldTypeTime timeSettings (entryTime <$> mEntry)
    (contentRes, contentView) <- mreq markdownField contentSettings (entryContent <$> mEntry)
    (authorsRes, authorsView) <- mreq (checkboxesField authorOpts) "Authors" $ Just authors
    now <- liftIO getCurrentTime

    let posted = case (dayRes, timeRes) of
            (FormSuccess (Just day), FormSuccess (Just time)) ->
                FormSuccess (Just (UTCTime day (timeOfDayToTime time)))

            (FormSuccess (Just _), FormSuccess Nothing) ->
                FormFailure ["Missing publication time"]

            (FormSuccess Nothing, FormSuccess (Just _)) ->
                FormFailure ["Missing publication date"]

            (FormSuccess Nothing, FormSuccess Nothing) ->
                FormSuccess Nothing

            _ -> FormFailure ["Could not parse time and/or date fields"]
        created = FormSuccess (maybe now entryCreated mEntry)
        entry = Entry <$> titleRes <*> contentRes <*> posted <*> created
        res = (,) <$> entry <*> authorsRes
        widget = $(widgetFile "admin-entry-form")

    return (res, widget)

entryDate :: Entry -> Maybe Day
entryDate e = utctDay <$> entryPosted e
entryTime :: Entry -> Maybe TimeOfDay
entryTime e = (timeToTimeOfDay . utctDayTime) <$> entryPosted e

titleSettings, publishNowSettings, dateSettings, timeSettings, contentSettings :: FieldSettings App
titleSettings = FieldSettings
    { fsLabel = "Title"
    , fsTooltip = Nothing
    , fsId = Just "title"
    , fsName = Just "title"
    , fsAttrs = [("placeholder", "Post Title")]
    }
publishNowSettings = FieldSettings
    { fsLabel = "Publish immediately"
    , fsTooltip = Nothing
    , fsId = Just "posted"
    , fsName = Just "posted"
    , fsAttrs = []
    }
dateSettings = FieldSettings
    { fsLabel = "Date"
    , fsTooltip = Nothing
    , fsId = Just "date"
    , fsName = Just "date"
    , fsAttrs = [("placeholder", "yyyy-mm-dd")]
    }
timeSettings = FieldSettings
    { fsLabel = "Time"
    , fsTooltip = Nothing
    , fsId = Just "time"
    , fsName = Just "time"
    , fsAttrs = [("placeholder", "hh:mm")]
    }
contentSettings = FieldSettings
    { fsLabel = "Content"
    , fsTooltip = Nothing
    , fsId = Just "content"
    , fsName = Just "content"
    , fsAttrs = [("placeholder", "Post Content")]
    }
