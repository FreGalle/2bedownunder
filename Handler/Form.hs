module Handler.Form
    ( wrap
    , entryForm
    , imageForm
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

imageForm :: Form (FileInfo, Maybe Text, UTCTime)
imageForm = renderDivs $ (,,)
    <$> fileAFormReq "Image file"
    <*> aopt textField "Image description" Nothing
    <*> lift (liftIO getCurrentTime)
