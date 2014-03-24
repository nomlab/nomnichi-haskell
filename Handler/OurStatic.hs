module Handler.OurStatic
  ( getOurStaticR       
  )
where

import Import
import Data.Time
import Data.List
import qualified Data.Text as DT
import System.Locale (defaultTimeLocale)
import System.IO.Unsafe (unsafePerformIO) 
import System.IO (readFile)
import Text.Blaze.Html (preEscapedToHtml)
import Data.ByteString.Char8 (unpack)
import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

readStaticFile :: FilePath -> Html
readStaticFile filePath = preEscapedToHtml $ unsafePerformIO $ readFile filePath

textsToFilePath :: [Text] -> FilePath
textsToFilePath texts = "public_html/" ++ ( init $ tail $ show $ DT.intercalate "/" texts )

getOurStaticR :: [Text] -> Handler Html
getOurStaticR path = do
    defaultLayout $ do
      [whamlet|#{readStaticFile $ textsToFilePath path}|]

