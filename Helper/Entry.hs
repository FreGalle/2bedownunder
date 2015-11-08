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
            msFencedHandlers = fences,
            msBlockFilter = noImgPs
        }
        fences = figureFenced `Map.union` columnFenced `Map.union` msFencedHandlers def
        noImgPs (pre : (BlockPara [img@InlineImage {}]:bs)) = pre : BlockPlainText [img]: noImgPs bs
        noImgPs (pre : (BlockPara [link@(InlineLink _ _ [InlineImage {}])]:bs)) = pre : BlockPlainText [link] : noImgPs bs
        noImgPs (h:hs) = h : noImgPs hs
        noImgPs [] = []

columnFenced :: Map Text (Text -> FencedHandler)
columnFenced = htmlFencedHandler
    "~~~"
    (\n -> "<div class=\"columns columns-" ++ n ++ "\">")
    (const "</div>")

figureFenced :: Map Text (Text -> FencedHandler)
figureFenced = htmlFencedHandler
    "@@@"
    (const "<figure>")
    (\description -> "<figcaption>" ++ description ++ "</figcaption></figure>")
