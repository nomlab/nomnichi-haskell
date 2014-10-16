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
import Data.Typeable (Typeable)
import Database.Persist
import Database.Persist.Sql (runMigration)
import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH
import Database.Persist.Quasi
import Control.Monad.IO.Class (liftIO)

-- 移行先データベースのスキーマ定義
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

-- これらはいらない
-- Article
--     memberName Text
-- Comment
--     commenter Text


-- 移行元tempデータベース
database1 = "temp_nomnichi.sqlite3"

-- 移行先データベース
database2 = "new_nomnichi.sqlite3"

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

  let insertArticle x = insertKey (entityKey x) $ (entityVal x)

  mapM_ insert $ fmap entityVal users
  mapM_ insertArticle articles
  mapM_ insert $ fmap entityVal comments

  return()
