module Handler.Entry where

import Import
import Helper.Entry
import Helper.Locale
import qualified Data.List as L

myGroupBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
myGroupBy f = map (f . L.head &&& id)
                   . groupBy ((==) `on` f)
                   . sortBy (compare `on` f)

getBlogEntriesR :: Handler Html
getBlogEntriesR = do
    now <- liftIO getCurrentTime
    entries <- runDB $ selectList [EntryPosted <. Just now] [Desc EntryPosted]
    let mMonthStr  = fmap (formatTime belgiumLocale "%B %_Y")
        mPosted = entryPosted . entityVal
        grouped = myGroupBy (mMonthStr . mPosted) entries
    defaultLayout $(widgetFile "entries")


getBlogEntryR :: EntryId -> Handler Html
getBlogEntryR entryId = do
    entryWithAuthors <- runDB $ selectEntryWithAuthors entryId
    case entryWithAuthors of
        [] -> notFound
        ((Entity _ entry,_):_) -> do
            (mPrev, mNext) <- selectPreviousNext entry
            isPublished <- isPublishedNow $ entryPosted entry
            let authors = map (entityVal . snd) entryWithAuthors
                contentMarkup = myMarkdown $ entryContent entry
            defaultLayout $(widgetFile "entry")
