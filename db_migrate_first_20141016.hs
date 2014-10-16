{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveDataTypeable         #-}

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time
import Data.List ((\\))
import Data.Typeable (Typeable)
import Database.Persist
import Database.Persist.Sql (runMigration)
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH
import Database.Persist.Quasi
import Control.Monad.IO.Class (liftIO)

-- 移行元と移行先両方のスキーマを定義
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    ident Text
    password Text Maybe
    salt Text
    UniqueUser ident
    deriving Typeable

Article
    memberName Text
    user UserId
    title Text
    permaLink Text
    content Html
    createdOn UTCTime
    updatedOn UTCTime
    publishedOn UTCTime
    approved Bool
    count Int
    promoteHeadline Bool
    deriving

Comment
    commenter Text
    user UserId
    body Textarea
    createdAt UTCTime
    updatedAt UTCTime
    articleId ArticleId
    deriving
|]

-- 移行元データベース
database1 = "nomnichi.sqlite3"

-- 移行先tempデータベース
database2 = "temp_nomnichi.sqlite3"

-- 移行元DBからデータを取得
getUsers = runSqlite database1 $ do
  users <- selectList [] [Asc UserId]
  return(users)

getArticles = runSqlite database1 $ do
  articles <- selectList [] [Asc ArticleId]
  return(articles)

getComments = runSqlite database1 $ do
  comments <- selectList [] [Asc CommentId]
  return(comments)

-- 移行処理
main :: IO ()
main = runSqlite database2 $ do
  runMigration migrateAll
  users <- getUsers
  articles <- getArticles
  comments <- getComments

  let setUserTuple = zip (fmap entityKey users) (fmap (userIdent . entityVal) users)

      getUserIdFromUserName name = findVal name setUserTuple

      findVal val [] = (read "Key {unKey = PersistInt64 0}") :: UserId
      findVal val ((k,v):xs)
        | val == v = k
        | otherwise = findVal val xs

      insertComment x = insert $ Comment {
        commentCommenter = commentCommenter x,
        commentUser = getUserIdFromUserName $ commentCommenter x,
        commentBody = commentBody x,
        commentCreatedAt = commentCreatedAt x,
        commentUpdatedAt = commentUpdatedAt x,
        commentArticleId = commentArticleId x
        }

      insertArticle x = insertKey (entityKey x) $ Article {
        articleMemberName = (articleMemberName . entityVal) x,
        articleUser = getUserIdFromUserName $ (articleMemberName . entityVal )x,
        articleTitle = (articleTitle . entityVal) x,
        articlePermaLink = (articlePermaLink . entityVal) x,
        articleContent = (articleContent . entityVal) x,
        articleCreatedOn = (articleCreatedOn . entityVal) x,
        articleUpdatedOn = (articleUpdatedOn . entityVal) x,
        articlePublishedOn = (articlePublishedOn . entityVal) x,
        articleApproved = (articleApproved . entityVal) x,
        articleCount = (articleCount . entityVal) x,
        articlePromoteHeadline = (articlePromoteHeadline . entityVal) x
        }

  mapM_ insert $ fmap entityVal users
  mapM_ insertArticle articles
  mapM_ insertComment $ fmap entityVal comments

  return()
