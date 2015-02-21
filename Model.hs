{-# LANGUAGE FlexibleInstances #-}

module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time
import Yesod.Auth.HashDB (HashDBUser(..))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity Article) where
  toJSON (Entity pid article) = object
                                [ "id"     .= toPathPiece pid
                                , "title"  .= articleTitle article
                                , "user"   .= articleUser article
                                ]

instance ToJSON (Entity User) where
  toJSON (Entity pid user) = object
                             [ "id"     .= toPathPiece pid
                             , "ident"  .= userIdent user
                             ]

instance HashDBUser User where
    userPasswordHash = userPassword
    userPasswordSalt = Just . userSalt
    setSaltAndPasswordHash s h p = p { userSalt = s,
                           userPassword = Just h
                           }
