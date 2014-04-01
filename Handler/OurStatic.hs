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

readStaticFile :: FilePath -> String
readStaticFile filePath = unsafePerformIO $ readFile filePath

textsToFilePath :: [Text] -> FilePath
textsToFilePath texts = "public_html/" ++ ( init $ tail $ show $ DT.intercalate "/" texts )

getOurStaticR :: [Text] -> Handler Html
getOurStaticR path = 
    case fileExtension of
-- 今後，以下の方法に書き換える
-- http://hackage.haskell.org/package/mime-types-0.1.0.0/docs/Network-Mime.html#t:FileName
      "html" -> defaultLayout $ do
                  [whamlet|#{preEscapedToHtml body}|]
      "png" -> sendFile typePng $ textsToFilePath path
      "jpg" -> sendFile typeJpeg $ textsToFilePath path
      _ -> sendFile typeOctet $ textsToFilePath path
    where 
      body = readStaticFile $ textsToFilePath path
      fileExtension = reverse $ fst $ break (=='.') $ reverse $ textsToFilePath path

