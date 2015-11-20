module Handler.Home where

import Import
import Handler.Entry

getHomeR :: Handler Html
getHomeR = getBlogEntriesR
