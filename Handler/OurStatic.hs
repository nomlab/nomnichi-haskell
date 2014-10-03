module Handler.OurStatic
  ( getOurStaticR       
  )
where

import Import
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
getOurStaticR path
    | fileExtension == filePath = defaultLayout $ [whamlet|#{preEscapedToHtml guessBody}|]
    | fileExtension == "html" = defaultLayout $ [whamlet|#{preEscapedToHtml body}|]
    | fileExtension == "png" = sendFile typePng filePath
    | fileExtension == "jpg" = sendFile typeJpeg filePath
    | otherwise = sendFile typeOctet filePath
-- 今後，以下の方法に書き換える
-- http://hackage.haskell.org/package/mime-types-0.1.0.0/docs/Network-Mime.html#t:FileName
    where 
        filePath = textsToFilePath path
        body = readStaticFile $ filePath
        guessBody = readStaticFile $ filePath ++ "/index.html"
        fileExtension = reverse $ fst $ break (=='.') $ reverse $ filePath

