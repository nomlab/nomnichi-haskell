{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import as I
import Data.Time
import Data.List as I (lines, unlines, isPrefixOf)
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    articles <- runDB $ selectList [ArticlePromoteHeadline ==. True, ArticleApproved ==. True] [Desc ArticleId]
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "乃村研究室ホームページ"
        $(widgetFile "homepage")


-- local functions --
takeHeadLine :: Html -> Html
takeHeadLine content = preEscapedToHtml $ prettyHeadLine $ renderHtml content

prettyHeadLine :: String -> String
prettyHeadLine article = gsub "_br_" "<br>" $ stripTags $ gsub "<br>" "_br_" $ I.unlines $ foldArticle $ I.lines article

stripTags' bool (x:xs)
  | xs   == []    = if x == '>'
                    then []
                    else [x]
  | bool == True  = if x == '>'
                    then stripTags' False xs
                    else stripTags' True xs
  | bool == False = if x == '<'
                    then stripTags' True xs
                    else x : (stripTags' False xs)
stripTags str = stripTags' False str

gsub _ _ [] = []
gsub x y str@(s:ss)
  | I.isPrefixOf x str = y ++ gsub x y (drop (length x) str)
  | otherwise = s:gsub x y ss

foldArticle :: [String] -> [String]
foldArticle lines = if lines == headLine
                    then take 3 lines
                    else headLine
  where headLine = foldAtFolding lines

foldAtFolding :: [String] -> [String]
foldAtFolding (x:xs)
  | x /= "<!-- folding -->" = if xs == []
                              then [x]
                              else x:foldAtFolding xs
  | x == "<!-- folding -->" = []
  | otherwise = []