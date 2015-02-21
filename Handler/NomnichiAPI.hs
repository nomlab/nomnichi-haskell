module Handler.NomnichiAPI
  ( getApiArticlesR    -- 記事一覧
  , getApiUsersR       -- ユーザ一覧
  )
where

import Import as I
import Data.Maybe()
import Yesod.Auth
import Yesod.Core.Json()
import Yesod.Form.Nic()

-- 記事一覧
getApiArticlesR :: Handler Value
getApiArticlesR = do
  creds <- maybeAuthId
  articles <- case creds of
    Just _ -> runDB $ selectList [] [Desc ArticlePublishedOn]
    Nothing -> runDB $ selectList [ArticleApproved ==. True] [Desc ArticlePublishedOn]
  returnJson $ toJSON articles

-- ユーザ一覧
getApiUsersR :: Handler Value
getApiUsersR = do
  users <- runDB $ selectList [] [Desc UserIdent]
  returnJson $ toJSON users
