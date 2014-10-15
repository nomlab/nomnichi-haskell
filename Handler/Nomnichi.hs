module Handler.Nomnichi
  ( getNomnichiR       -- ノムニチトップ
  , getCreateArticleR  -- 記事投稿ページの表示
  , postCreateArticleR -- 記事の投稿
  , getArticleR        -- 記事の表示
  , postArticleR       -- 記事の編集
  , getEditArticleR    -- 記事の編集画面の表示
  , postDeleteArticleR -- 記事の削除
  , postCommentR       -- コメントの投稿
  )
where

import Import as I
import Data.List as I (isPrefixOf)
import Data.Text as T (append, pack, unpack)
import Data.Time
import qualified Data.Time.Format()
import Data.Maybe()
import System.Locale (defaultTimeLocale)
import System.IO.Unsafe (unsafePerformIO)
import Yesod.Auth
import Text.Blaze.Html (preEscapedToHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Yesod.Form.Nic()

-- 記事作成，閲覧，更新
-- ノムニチトップ
getNomnichiR :: Handler Html
getNomnichiR = do
  creds <- maybeAuthId
  articles <- case creds of
    Just _ -> runDB $ selectList [] [Desc ArticleId]
    Nothing -> runDB $ selectList [ArticleApproved ==. True] [Desc ArticleId]
  users <- sequence $ fmap (\x -> articleAuthorName x) articles
  paramPage <- lookupGetParam "page"
  let zippedArticles = I.zip articles users
      articlesOnPage = takeArticlesOnPage calcPageNumber zippedArticles
      calcPageNumber = case paramPage of
                     Just page -> if ((convTextToInt page) < minPageNumber) ||
                                     ((convTextToInt page) > calcMaxPageNumber)
                                  then minPageNumber
                                  else convTextToInt page
                     Nothing -> 1
      minPageNumber = 1
      calcMaxPageNumber = (+1) $ div (I.length articles) perPage
      linkToOtherPageNumber pageNumber =
        [hamlet|
        <a href=@{HomeR}/nomnichi?page=1><<</a>
        $forall displayPageNumber <- displayPageNumbers pageNumber calcMaxPageNumber
          $if pageNumber == displayPageNumber
            &nbsp;#{show displayPageNumber}
          $else
            &nbsp;<a href=@{HomeR}/nomnichi?page=#{show displayPageNumber}>#{show displayPageNumber}</a>
        <a href=@{HomeR}/nomnichi?page=#{calcMaxPageNumber}>&nbsp;>></a>
        |]
      displayPageNumbers pageNumber maxPageNumber
        | pageNumber > 5  = if maxPageNumber > (pageNumber + 9)
                            then I.take 10 [(pageNumber - 4)..]
                            else [(pageNumber - 4)..maxPageNumber]
        | otherwise       = if maxPageNumber > 10
                            then I.take 10 [1..]
                            else [1..maxPageNumber]
  case articles of
    [] -> defaultLayout [whamlet|
                        <div class="home">
                          <h1 class="home"> Articles
                          <p> There are no articles in the blog.
                        $maybe _ <- creds
                          <a href=@{HomeR}/nomnichi/create> Create Article
                          <br>
                          <a href=@{HomeR}/auth/logout> Logout
                        $nothing
                        |]
    _ -> defaultLayout $ do
           $(widgetFile "articles")
   where
     takeArticlesOnPage pageNumber zippedArticles =
       I.drop (calcNumOfDroppingArticles pageNumber)
       $ I.take (calcNumOfArticles pageNumber) zippedArticles
     calcNumOfArticles pageNumber = perPage * pageNumber
     calcNumOfDroppingArticles pageNumber = perPage * (pageNumber - 1)
     lockedImg article =
       case articleApproved article of
         True -> [hamlet||]
         _    -> [hamlet|<img src="/lab/nom/static/img/lock.png" width="20px" height="20px">|]
     displayLinksforLoginedMember creds =
       case creds of
         (Just _) -> [hamlet|
                     <a href=@{HomeR}/nomnichi/create> Create Article
                     <br>
                     <a href=@{HomeR}/auth/logout> Logout
                     |]
         _        -> [hamlet||]

articleAuthorName :: Entity Article -> Handler (Maybe User)
articleAuthorName (Entity _ article) = do
  runDB $ get (articleUser article)

displayAuthorName :: Maybe User -> Text
displayAuthorName (Just user) = userIdent user
displayAuthorName Nothing     = "Unknown user"

convTextToInt :: Text -> Int
convTextToInt text = read $ T.unpack text :: Int

getCreateArticleR :: Handler Html
getCreateArticleR = do
  userId <- requireAuthId
  user <- runDB $ get404 userId
  let format = "%Y%m%d-%H%M%S"
      utcToNomnichiTime = utcToLocalTime $ unsafePerformIO getCurrentTimeZone
      permaLinkTime = T.pack $ formatTime defaultTimeLocale format $ utcToNomnichiTime $ unsafePerformIO getCurrentTime
      permaLink = (userIdent user) `T.append` ("-" :: Text) `T.append` permaLinkTime
  (articleWidget, enctype) <- generateFormPost $ entryForm permaLink
  defaultLayout $ do
    $(widgetFile "createArticleForm")

-- 記事作成
postCreateArticleR :: Handler Html
postCreateArticleR = do
  ((res, articleWidget), enctype) <- runFormPost $ entryForm ("" :: Text)
  case res of
    FormSuccess article -> do
      articleId <- runDB $ insert article
      setMessage $ toHtml (articleTitle article) <> " created."
      redirect $ ArticleR articleId
    _ -> defaultLayout $ do
      setTitle "Please correct your entry form."
      $(widgetFile "articleAddError")

-- 記事表示
getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
  creds    <- maybeAuthId
  article  <- runDB $ get404 articleId
  user     <- runDB $ get (articleUser article)
  comments <- runDB $ selectList [CommentArticleId ==. articleId] [Asc CommentId]
  (commentWidget, enctype) <- generateFormPost $ commentForm articleId
  case creds of
    Just _ ->
      defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "authedArticle")
    Nothing ->
      case articleApproved article of
        True ->
          defaultLayout $ do
            setTitle $ toHtml $ articleTitle article
            $(widgetFile "article")
        False ->
          defaultLayout $ do
          redirect $ NomnichiR

-- 記事更新
postArticleR :: ArticleId -> Handler Html
postArticleR articleId = do
  beforeArticle <- runDB $ get404 articleId
  ((res, articleWidget), enctype) <- runFormPost (editForm (Just beforeArticle))
  case res of
    FormSuccess article -> do
      runDB $ do
        update articleId
          [ ArticleUser            =. articleUser article
          , ArticleTitle           =. articleTitle article
          , ArticlePermaLink       =. articlePermaLink article
          , ArticleContent         =. articleContent article
          , ArticleUpdatedOn       =. articleUpdatedOn article
          , ArticlePublishedOn     =. articlePublishedOn article
          , ArticleApproved        =. articleApproved article
          , ArticlePromoteHeadline =. articlePromoteHeadline article
          ]
      setMessage $ toHtml $ (articleTitle article) <> " is updated."
      redirect $ ArticleR articleId
    _ -> defaultLayout $ do
      setTitle "Please correct your entry form."
      $(widgetFile "editArticleForm")

-- 編集画面
getEditArticleR :: ArticleId -> Handler Html
getEditArticleR articleId = do
  article <- runDB $ get404 articleId
  (articleWidget, enctype) <- generateFormPost $ editForm (Just article)
  defaultLayout $ do
    $(widgetFile "editArticleForm")


-- 記事削除

postDeleteArticleR :: ArticleId -> Handler Html
postDeleteArticleR articleId = do
  runDB $ do
    delete articleId
    deleteWhere [ CommentArticleId ==. articleId ]
  setMessage "successfully deleted."
  redirect $ NomnichiR


-- コメント



-- コメント送信
postCommentR :: ArticleId -> Handler Html
postCommentR articleId = do
  ((res, _), _) <- runFormPost $ commentForm articleId
  case res of
    FormSuccess comment -> do
      _ <- runDB $ insert comment
      setMessage "your comment was successfully posted."
      redirect $ ArticleR articleId
    _ -> do
      setMessage "please fill up your comment form."
      redirect $ ArticleR articleId

-- 記事表示時の公開時刻の整形
formatToNomnichiTime :: Article ->  String
formatToNomnichiTime article = formatTime defaultTimeLocale format $ utcToNomnichiTime $ articlePublishedOn article
  where format = "%Y/%m/%d (%a)  %H:%M"
        utcToNomnichiTime = utcToLocalTime $ unsafePerformIO getCurrentTimeZone

-- コメント投稿時刻の整形
formatToCommentTime :: Comment ->  String
formatToCommentTime comment = formatTime defaultTimeLocale format $ utcToNomnichiTime $ commentCreatedAt comment
  where format = "%Y/%m/%d (%a)  %H:%M"
        utcToNomnichiTime = utcToLocalTime $ unsafePerformIO getCurrentTimeZone


-- フォーム
entryForm :: Text -> Form Article
entryForm permaLink = renderDivs $ Article
  <$> lift requireAuthId
  <*> areq textField    "Title"           Nothing
  <*> areq textField    "PermaLink"       (Just permaLink)
  <*> areq htmlField    "Content"         Nothing
  <*> lift (liftIO getCurrentTime) -- CreatedOn
  <*> lift (liftIO getCurrentTime) -- UpdatedOn
  <*> areq utcTimeField "PublishedOn"     (Just (unsafePerformIO getCurrentTime))
  <*> areq boolField    "Approved"        (Just False)
  <*> pure 0 -- Count
  <*> areq boolField    "PromoteHeadline" (Just False)

editForm :: Maybe Article -> Form Article
editForm article = renderDivs $ Article
  <$> pure (nonMaybeUserId                (articleUser <$> article))
  <*> areq textField    "Title"           (articleTitle <$> article)
  <*> pure (nonMaybeText                  (articlePermaLink <$> article))
  <*> areq htmlField    "Content"         (articleContent <$> article)
  <*> pure (nonMaybeUTCTime               (articleCreatedOn <$> article))
  <*> lift                                (liftIO getCurrentTime) -- UpdatedOn
  <*> areq utcTimeField "PublishedOn"     (articlePublishedOn <$> article)
  <*> areq boolField    "Approved"        (articleApproved <$> article)
  <*> pure (nonMaybeInt                   (articleCount <$> article))
  <*> areq boolField    "PromoteHeadline" (articlePromoteHeadline <$> article)

nonMaybeUserId :: Maybe UserId -> UserId
nonMaybeUserId (Just uid) = uid
nonMaybeUserId Nothing    = (read "Key {unKey = PersistInt64 0}") :: UserId

nonMaybeInt :: Num num => Maybe num -> num
nonMaybeInt (Just num) = num
nonMaybeInt Nothing    = 0

nonMaybeText :: Maybe Text -> Text
nonMaybeText (Just text) = text
nonMaybeText Nothing     = "" :: Text

nonMaybeUTCTime :: Maybe UTCTime -> UTCTime
nonMaybeUTCTime (Just utctime) = utctime
nonMaybeUTCTime Nothing        = (read "1970-01-01 00:00:00.0 UTC") :: UTCTime

commentForm :: ArticleId -> Form Comment
commentForm articleId = renderDivs $ Comment
  <$> areq textField     "Name"    Nothing
  <*> areq textareaField "Comment" Nothing
  <*> lift (liftIO getCurrentTime)
  <*> lift (liftIO getCurrentTime)
  <*> pure articleId

takeHeadLine :: Html -> Html
takeHeadLine content = preEscapedToHtml $ prettyHeadLine $ renderHtml content

prettyHeadLine :: String -> String
prettyHeadLine article = gsub "_br_" "<br>" $ stripTags $ gsub "<br>" "_br_" $ foldArticle article

stripTags :: String -> String
stripTags str = stripTags' False str
stripTags' :: Bool -> String -> String
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
  | I.isPrefixOf x str = y ++ gsub x y (I.drop (I.length x) str)
  | otherwise = s:gsub x y ss

defaultNumOfLines :: Int
defaultNumOfLines = 3

perPage :: Int
perPage = 10

foldArticle :: String -> String
foldArticle content = case foldAtFolding content of
                           Just value -> value
                           Nothing -> I.unlines $ I.take defaultNumOfLines $ I.lines content

foldAtFolding :: String -> Maybe String
foldAtFolding content = if (I.length splitContent) > 1
                        then Just $ I.head splitContent
                        else Nothing
  where splitContent = split "<!-- folding -->" content

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

utcTimeField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m UTCTime
utcTimeField = Field
     { fieldParse = parseHelper parseTime'
     , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<input id="#{theId}" name="#{name}" type="datetime" *{attrs} :isReq:required="" value="#{showVal val}">
|]
     , fieldEnctype = UrlEncoded
     }
    where
       showVal = either id (pack . formatTime defaultTimeLocale "%F %T")

parseTime' :: Text -> Either FormMessage UTCTime
parseTime' theText =
    maybe
       (Left MsgInvalidTimeFormat)
       (\x -> Right x)
       (Data.Time.parseTime defaultTimeLocale "%F %T" $ unpack theText)
