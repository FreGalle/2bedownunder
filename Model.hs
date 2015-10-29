module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Text.Markdown (Markdown)
import Yesod.Text.Markdown ()
import Data.Aeson.TH (deriveJSON, defaultOptions)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

data ImageUpdate = ImageUpdate
    { visible :: Bool
    , description :: Maybe Textarea
    }
$(deriveJSON defaultOptions ''ImageUpdate)
