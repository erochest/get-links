{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Main where


import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Data.Bifunctor
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C8
import           Data.CSV.Conduit
import qualified Data.List             as L
import qualified Data.Map.Lazy         as M
import qualified Data.Text             as T
import           Data.Text.Encoding    (encodeUtf8)
import           Database.MySQL.Simple
import           Network.Wreq          hiding (header)
import           System.IO
import           Text.HTML.TagSoup


type RecordInfo = (Int, Maybe T.Text, T.Text)
type FullRecord = (Int, Maybe T.Text, T.Text, Bool)


login :: ConnectInfo
login = defaultConnectInfo { connectDatabase = "intersections" }

sql :: Query
sql = "SELECT id, title, body \
      \FROM omeka_neatline_records \
      \WHERE body LIKE '%jpg%' OR body LIKE '%png%';"

output :: FilePath
output = "intersections-2.csv"

header :: (B.ByteString, B.ByteString, B.ByteString, B.ByteString)
header = ("Record ID", "Title", "URL", "Exists")


getBodies :: IO [RecordInfo]
getBodies = do
    cxn <- connect login
    query_ cxn sql <* close cxn


getImages :: T.Text -> [T.Text]
getImages = filter (not . T.null)
          . map (fromAttrib "src")
          . filter (isTagOpenName "img")
          . parseTags


spreadLast :: (a, b, [c]) -> [(a, b, c)]
spreadLast (a, b, cs) = map (a, b,) cs

pingURL :: RecordInfo -> IO FullRecord
pingURL (a, b, url) = do
    putStr $ "Testing " ++ url' ++ " ... "
    status <- catch ((^. responseStatus. statusCode) <$> get url')
                    ((const $ return 404) :: SomeException -> IO Int)
    putStrLn $ "status = " ++ show status
    return (a, b, url, status == 200)
    where
        url' = T.unpack url


toMapRows :: (B.ByteString, B.ByteString, B.ByteString, B.ByteString)
          -> [FullRecord]
          -> [MapRow B.ByteString]
toMapRows (f1, f2, f3, f4) = (hrow :) . map row
    where
        hrow = M.fromList [ (f1, f1)
                          , (f2, f2)
                          , (f3, f3)
                          ]
        row (v1, v2, v3, v4) = M.fromList [ (f1, cshow v1)
                                          , (f2, maybe "" encodeUtf8 v2)
                                          , (f3, encodeUtf8 v3)
                                          , (f4, cshow v4)
                                          ]
        cshow :: Show a => a -> B.ByteString
        cshow = C8.pack . show

writeOutput :: FilePath
            -> [MapRow B.ByteString]
            -> IO ()
writeOutput filename = writeCSVFile defCSVSettings filename WriteMode


main :: IO ()
main =   writeOutput output
     .   toMapRows header
     =<< mapM pingURL
     .   L.concatMap (spreadLast . second getImages)
     =<< getBodies
