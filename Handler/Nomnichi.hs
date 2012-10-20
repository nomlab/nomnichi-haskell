module Handler.Nomnichi
  ( getNomnichiR
  , postNomnichiR
  , getArticleR
  , postArticleR
  , getEditArticleR
  , getDeleteArticleR
  )
where

import Import
import Data.Monoid
import Data.Time
import System.Locale (defaultTimeLocale)
import Settings
import Data.Maybe

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App


-- 記事作成，閲覧，更新 -------------------------------------------

entryForm :: Form Article
entryForm = renderDivs $ Article
  <$> areq textField    "Title"   Nothing
  <*> areq nicHtmlField "Content" Nothing
  <*> aformM (liftIO getCurrentTime)
  <*> aformM (liftIO getCurrentTime)
  <*> aformM (liftIO getCurrentTime)

-- ノムニチトップページ
getNomnichiR :: Handler RepHtml
getNomnichiR = do
  articles <- runDB $ selectList [] [Desc ArticleId]
  (articleWidget, enctype) <- generateFormPost entryForm
  defaultLayout $ do
    $(widgetFile "articles")

-- 記事作成
postNomnichiR :: Handler RepHtml
postNomnichiR = do
  ((res, articleWidget), enctype) <- runFormPost entryForm
  case res of
    FormSuccess article -> do
      articleId <- runDB $ insert article
      setMessage $ toHtml (articleTitle article) <> " created."
      redirect $ ArticleR articleId
    _ -> defaultLayout $ do
      setTitle "Please correct your entry form."
      $(widgetFile "articleAddError")

-- 記事表示
getArticleR :: ArticleId -> Handler RepHtml
getArticleR articleId = do
  article <- runDB $ get404 articleId
  defaultLayout $ do
    setTitle $ toHtml $ articleTitle article
    $(widgetFile "article")

-- 記事更新
postArticleR :: ArticleId -> Handler RepHtml
postArticleR articleId = do
  ((res, articleWidget), enctype) <- runFormPost (editForm Nothing)
  case res of
    FormSuccess article -> do
      runDB $ do
        _article <- get404 articleId
        update articleId
          [ ArticleTitle   =. articleTitle   article
          , ArticleContent =. articleContent article
          , ArticleUpdatedOn =. articleUpdatedOn article
          ]
      setMessage $ toHtml $ (articleTitle article) <> " is updated."
      redirect $ ArticleR articleId
    _ -> defaultLayout $ do
      setTitle "Please correct your entry form."
      $(widgetFile "editArticleForm")

-- 編集 -------------------------------------------

-- 編集画面
getEditArticleR :: ArticleId -> Handler RepHtml
getEditArticleR articleId = do
  article <- runDB $ get404 articleId
  (articleWidget, enctype) <- generateFormPost $ editForm (Just article)
  defaultLayout $ do
    $(widgetFile "editArticleForm")

editForm :: Maybe Article -> Form Article
editForm article = renderDivs $ Article
  <$> areq textField    "Title"   (articleTitle <$> article)
  <*> areq nicHtmlField "Content" (articleContent <$> article)
  <*> aformM (liftIO getCurrentTime)
  <*> aformM (liftIO getCurrentTime)
  <*> aformM (liftIO getCurrentTime)


-- 記事削除 -------------------------------------------

getDeleteArticleR :: ArticleId -> Handler RepHtml
getDeleteArticleR articleId = do
  runDB $ do
    _post <- get404 articleId
    delete articleId
  setMessage "successfully deleted."
  redirect $ NomnichiR


-- 時刻 -------------------------------------------

formatToNomnichiTime :: Article ->  String
formatToNomnichiTime article = formatTime defaultTimeLocale format $ utcToNomnichiTime $ articlePublishedOn article
  where format = "%Y/%m/%d (%a)  %H:%M"
        utcToNomnichiTime = (utcToLocalTime timeZone)
