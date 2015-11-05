module Helper.Entry
    ( myMarkdown ) where

import Import

import qualified Data.Map as Map
import Text.Markdown
    ( Markdown (..)
    , MarkdownSettings (..)
    , FencedHandler
    , htmlFencedHandler, markdown
    )
import Text.Markdown.Block (Block(..))
import Text.Markdown.Inline (Inline(..))
import Text.Blaze.Html ()

myMarkdown :: Markdown -> Html
myMarkdown (Markdown text) = markdown settings text
    where
        settings = def {
            msFencedHandlers = figureFenced `Map.union` msFencedHandlers def,
            msBlockFilter = noImgPs
        }
        noImgPs (pre : (BlockPara [img@InlineImage {}]:bs)) = pre : BlockPlainText [img]: noImgPs bs
        noImgPs (h:hs) = h : noImgPs hs
        noImgPs [] = []

figureFenced :: Map Text (Text -> FencedHandler)
figureFenced = htmlFencedHandler
    "@@@"
    (const "<figure>")
    (\description -> "<figcaption>" ++ description ++ "</figcaption></figure>")
