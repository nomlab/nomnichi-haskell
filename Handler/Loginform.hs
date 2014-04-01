module Handler.Loginform
    ( getLoginformR
    , postLoginformR
    )
where
import Import
import Data.Monoid()

getLoginformR :: Handler Html
getLoginformR = do
    deleteSession "loginname"
    id <- return ("" :: Text)
    defaultLayout $ do
        $(widgetFile "loginform")

postLoginformR :: Handler Html
postLoginformR = do
     -- template/loginform のフォーム:UserIdを取得し，データベースと比較
     loginname <- runInputPost $ ireq textField "UserId"
     passwd <- runInputPost $ iopt textField "Password"
     user <- runDB $ selectList [(LoginuserIdstr ==. loginname), (LoginuserPassword ==. passwd)] [Desc LoginuserId]
     case user of
           [] ->do 
            setMessage "login ERROR!"
            id <- return loginname --再度ログインページを表示する際に，IDフォームの中身を残す
            defaultLayout $ do
                   $(widgetFile "loginform")
           _ -> do       -- 一致した場合
              -- POST された名前を取得し、セッションに設定する
              loginname <- runInputPost $ ireq textField "UserId"
              setSession "loginname" loginname
              -- その後、最終転送先orデフォルトへリダイレクトする
              redirectUltDest HomeR