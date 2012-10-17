module Handler.Loginform
    ( getLoginformR
    , postLoginformR
    )
where

import Import
import Data.Monoid()

-- to use Html into forms
-- import Yesod.Form.Nic (YesodNic, nicHtmlField)
-- instance YesodNic App

loginForm :: Form Loginuser
loginForm = renderDivs $ Loginuser
    <$> areq   textField "UserId" Nothing
    <*> aopt   textField "Password" Nothing

getLoginformR :: Handler RepHtml
getLoginformR = do
    deleteSession "loginname"
    defaultLayout $ do 
        $(widgetFile "loginform")

postLoginformR :: Handler RepHtml
postLoginformR = do
     -- template/loginform のフォーム:UserIdを取得し，データベースと比較
     loginname <- runInputPost $ ireq textField "UserId"
     user <- runDB $ selectList [LoginuserIdstr ==. loginname] [Desc LoginuserId]

     case user of
           [] -> do    -- 一致しない場合
              redirectUltDest HomeR
           _ -> do       -- 一致した場合
              -- POST された名前を取得し、セッションに設定する
              loginname <- runInputPost $ ireq textField "UserId"
              setSession "loginname" loginname
              -- その後、最終転送先orデフォルトへリダイレクトする
              redirectUltDest HomeR

--    ((res,loginuserWidget), enctype) <- runFormPost makingForm
--    case res of
--         FormSuccess loginuser -> do 
--            loginuserId <- runDB $ insert loginuser 
--            setMessage $ toHtml $ (loginuserIdstr loginuser) <> " created" 
--            redirect $ UserR
--         _ -> defaultLayout $ do
--                setTitle "Please correct your entry form"
--                $(widgetFile "userAddError")


-- postNewuserR :: ArticleId -> Handler RepHtml
-- postNewuserR articleId = do
--    article <- runDB $ get404 articleId
--    defaultLayout $ do
--        setTitle $ toHtml $ articleTitle article
--        $(widgetFile "article")