module Handler.OurStatic
  ( getOurStaticR       
  )
where

import Import
import Data.Time
import Data.List
import qualified Data.Text as DT
import System.IO.Unsafe (unsafePerformIO) 
import System.IO (readFile)
import Text.Blaze.Html (preEscapedToHtml)


readStaticFile :: FilePath -> String
readStaticFile filePath = unsafePerformIO $ readFile filePath

textsToFilePath :: [Text] -> FilePath
textsToFilePath texts = "public_html/" ++ ( init $ tail $ show $ DT.intercalate "/" texts )

getOurStaticR :: [Text] -> Handler Html
getOurStaticR path = 
    if fileExtension == filePath
    then 
        defaultLayout $ [whamlet|#{preEscapedToHtml guessBody}|]
    else                                               
        case fileExtension of
-- 今後，以下の方法に書き換える
-- http://hackage.haskell.org/package/mime-types-0.1.0.0/docs/Network-Mime.html#t:FileName
               "html" -> defaultLayout $ do
                  [whamlet|#{preEscapedToHtml body}|]
               "png" -> sendFile typePng filePath
               "jpg" -> sendFile typeJpeg filePath
               _ -> sendFile typeOctet filePath
    where 
      filePath = textsToFilePath path
      body = readStaticFile $ filePath
      guessBody = readStaticFile $ filePath ++ "/index.html"
      fileExtension = reverse $ fst $ break (=='.') $ reverse $ filePath

