module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Text.Markdown (Markdown)
import Yesod.Text.Markdown ()
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Time.Clock (diffUTCTime)
import Yesod.Auth.HashDB (HashDBUser(..))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance HashDBUser User where
    userPasswordHash = userPassword
    setPasswordHash h u = u { userPassword = Just h }

data ImageUpdate = ImageUpdate
    { visible :: Bool
    , description :: Maybe Textarea
    }
$(deriveJSON defaultOptions ''ImageUpdate)

isPublishedNow :: MonadIO m => Maybe UTCTime -> m Bool
isPublishedNow published = case published of
    Nothing -> return False
    Just publishAt -> do
        now <- liftIO getCurrentTime
        let diff = diffUTCTime now publishAt
            hasBeenPublished = negate diff < diff
        return hasBeenPublished
