module Handler.Nomnichi
  ( getNomnichiR
  , postNomnichiR
  , getArticleR
  , putArticleR
  , deleteArticleR
  , getEditArticleR
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

entryForm :: Form Article
entryForm = renderDivs $ Article
  <$> areq textField    "Title"   Nothing
  <*> areq nicHtmlField "Content" Nothing
  <*> aformM (liftIO getCurrentTime)
  <*> aformM (liftIO getCurrentTime)
  <*> aformM (liftIO getCurrentTime)

editForm :: Maybe Article -> Form Article
editForm article = renderDivs $ Article
  <$> areq textField    "Title"   (articleTitle <$> article)
  <*> areq nicHtmlField "Content" (articleContent <$> article)
  <*> aformM (liftIO getCurrentTime)
  <*> aformM (liftIO getCurrentTime)
  <*> aformM (liftIO getCurrentTime)

getNomnichiR :: Handler RepHtml
getNomnichiR = do
  articles <- runDB $ selectList [] [Desc ArticleId]
  (articleWidget, enctype) <- generateFormPost entryForm
  defaultLayout $ do
    $(widgetFile "articles")

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

getArticleR :: ArticleId -> Handler RepHtml
getArticleR articleId = do
  article <- runDB $ get404 articleId
  defaultLayout $ do
    setTitle $ toHtml $ articleTitle article
    $(widgetFile "article")

deleteArticleR :: ArticleId -> Handler RepHtml
deleteArticleR articleId = do
  runDB $ do
    _article <- get404 articleId
    delete articleId
  setMessage "successfully deleted."
  redirect NomnichiR

putArticleR :: ArticleId -> Handler RepHtml
putArticleR articleId = do
  ((res, articleWidget), enctype) <- runFormPost (editForm Nothing)
  case res of
    FormSuccess article -> do
      runDB $ do
        _article <- get404 articleId
        update articleId
          [ ArticleTitle   =. articleTitle   article
          , ArticleContent =. articleContent article
          , ArticleUpdated_on =. articleUpdated_on article
          ]
      setMessage $ toHtml $ (articleTitle article) <> " is updated."
      redirect $ ArticleR articleId
    _ -> defaultLayout $ do
      setTitle "Please correct your entry form."
      $(widgetFile "editArticleForm")

getEditArticleR :: ArticleId -> Handler RepHtml
getEditArticleR articleId = do
  article <- runDB $ get404 articleId
  (articleWidget, enctype) <- generateFormPost $ editForm (Just article)
  defaultLayout $ do
    $(widgetFile "editArticleForm")


formatToNomnichiTime :: Article ->  String
formatToNomnichiTime article = formatTime defaultTimeLocale format $ utcToNomnichiTime $ articlePublished_on article
  where format = "%Y/%m/%d (%a)  %H:%M"
        utcToNomnichiTime = (utcToLocalTime timeZone)
