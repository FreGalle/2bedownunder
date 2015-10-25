module Handler.Form
    ( wrap
    , entryForm
    ) where

import Import
import Yesod.Text.Markdown (markdownField)

wrap :: Widget -> Enctype -> Widget
wrap widget enctype = do
    [whamlet|
<form method=post enctype=#{enctype}>
    ^{widget}
    <div>
        <input type=submit value=Submit>
|]


entryForm :: Form Entry
entryForm = renderDivs $ Entry
    <$> areq textField "Title" Nothing
    <*> areq markdownField "Content" Nothing
    <*> lift (liftIO getCurrentTime)
