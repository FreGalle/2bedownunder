-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import
import Yesod.Sitemap (sitemap, SitemapUrl (..), SitemapChangeFreq (..))
import qualified Data.Conduit.List as CL

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

getSitemapR :: Handler TypedContent
getSitemapR = do
    now <- liftIO getCurrentTime
    sitemap $ runDBSource $ do
        yield $ SitemapUrl HomeR Nothing (Just Daily) (Just 1.0)

        yield $ SitemapUrl BlogEntriesR Nothing (Just Daily) (Just 0.8)
        yield $ SitemapUrl BlogImagesR Nothing (Just Daily) (Just 0.8)

        selectSource [EntryPosted <. Just now] [Asc EntryPosted] $= CL.mapMaybeM (\(Entity eId _) ->
            return $ Just $ SitemapUrl (BlogEntryR eId) Nothing (Just Weekly) (Just 0.5))
