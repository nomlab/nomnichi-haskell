module Handler.Usermanagement
    ( getUserR
--    , postUseridR
    , postUserR
    )
where

import Import
import Data.Monoid()

-- to use Html into forms
-- import Yesod.Form.Nic (YesodNic, nicHtmlField)
-- instance YesodNic App

makingForm :: Form Loginuser
makingForm = renderDivs $ Loginuser
    <$> areq   textField "UserId" Nothing
    <*> aopt   textField "Password" Nothing

-- The view showing the list of users
getUserR :: Handler RepHtml
getUserR = do
    -- Get the list of articles inside the database.
    loginusers <- runDB $ selectList [] [Desc LoginuserIdstr]
    -- We'll need the two "objects": usersWidget and password
    -- to construct the form (see templates/loginusers.hamlet).
    (loginuserWidget, enctype) <- generateFormPost makingForm
    defaultLayout $ do
        $(widgetFile "loginusers")

-- we continue Handler/Blog.hs
postUserR :: Handler RepHtml
postUserR = do
    ((res,loginuserWidget), enctype) <- runFormPost makingForm
    case res of
         FormSuccess loginuser -> do 
            loginuserId <- runDB $ insert loginuser 
            setMessage $ toHtml $ (loginuserIdstr loginuser) <> " created" 
            redirect $ UserR
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "userAddError")


-- postNewuserR :: ArticleId -> Handler RepHtml
-- postNewuserR articleId = do
--    article <- runDB $ get404 articleId
--    defaultLayout $ do
--        setTitle $ toHtml $ articleTitle article
--        $(widgetFile "article")