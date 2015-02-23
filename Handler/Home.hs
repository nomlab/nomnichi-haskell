{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import as I
import Data.Time
import Data.List as I (isPrefixOf)
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Yesod.Auth (requireAuthId)
import Yesod.Auth.HashDB (setSaltAndPasswordHash)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Text as T (append, pack, unpack)
import Data.ByteString.Lazy.Char8 as BS (pack)
import Data.Maybe

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    articles <- runDB $ selectList [ArticlePromoteHeadline ==. True, ArticleApproved ==. True] [Desc ArticleId]
    users <- sequence $ fmap (\x -> articleAuthorName x) articles
    let zippedArticles = I.zip articles users
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "乃村研究室ホームページ"
        $(widgetFile "homepage")

getChangePassR :: Handler Html
getChangePassR = do
    userId <- requireAuthId
    user <- runDB $ get404 userId
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "パスワード変更"
        $(widgetFile "changePass")

postChangePassR :: Handler Html
postChangePassR = do
    userId <- requireAuthId
    user <- runDB $ get404 userId
    let salt = userSalt user
    inputPassword <- runInputPost $ ireq textField "password"
    runDB $ do
      update userId
          [ UserPassword =. (Just $ saltedHash salt inputPassword) ]
    setMessage $ toHtml $ (userIdent user) <> " is updated."
    redirect $ HomeR

-- local functions --
articleAuthorName :: Entity Article -> Handler (Maybe User)
articleAuthorName (Entity _ article) = do
  runDB $ get (articleUser article)

displayAuthorName :: Maybe User -> Text
displayAuthorName (Just user) = userIdent user
displayAuthorName Nothing     = "Unknown user"

takeHeadLine :: Html -> Html
takeHeadLine content = preEscapedToHtml $ prettyHeadLine $ renderHtml content

prettyHeadLine :: String -> String
prettyHeadLine article = gsub "_br_" "<br>" $ stripTags $ gsub "<br>" "_br_" $ foldArticle article

stripTags :: [Char] -> [Char]
stripTags str = stripTags' False str
stripTags' :: Bool -> [Char] -> [Char]
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
  | otherwise     = [] -- maybe don't occur

gsub :: Eq a => [a] -> [a] -> [a] -> [a]
gsub _ _ [] = []
gsub x y str@(s:ss)
  | I.isPrefixOf x str = y ++ gsub x y (drop (length x) str)
  | otherwise = s:gsub x y ss

foldArticle :: String -> String
foldArticle content = case foldAtFolding content of
                           Just value -> value
                           Nothing -> I.unlines $ I.take defaultNumOfLines $ I.lines content

foldAtFolding :: String -> Maybe String
foldAtFolding content = if (I.length splitContent) > 1
                        then Just $ I.head splitContent
                        else Nothing
  where splitContent = split "<!-- folding -->" content

defaultNumOfLines :: Int
defaultNumOfLines = 3

numOfNewArticles :: Int
numOfNewArticles = 3

-- We want to import Data.List.Utils (split), but...
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (startswith delim) str
        in
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim
                                                 (drop (length delim) x)

startswith :: Eq a => [a] -> [a] -> Bool
startswith = isPrefixOf

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

saltedHash :: Text -> Text -> Text
saltedHash salt = T.pack . showDigest . sha1 . BS.pack . T.unpack . T.append salt
