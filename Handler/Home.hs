module Handler.Home where

import Import
import Handler.Entry

getHomeR :: Handler Html
getHomeR = do
    now <- liftIO getCurrentTime
    mEntryId <- runDB $ selectFirst [EntryPosted <. Just now] [Desc EntryPosted]
    case mEntryId of
        Just (Entity entryId _) -> getBlogEntryR entryId
        Nothing -> getBlogEntriesR
