{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Main where


import           Control.Applicative
import           Data.Bifunctor
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as C8
import           Data.CSV.Conduit
import qualified Data.List                 as L
import qualified Data.Map.Lazy             as M
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8)
import           Database.MySQL.Simple
import           Options.Applicative       hiding (header)
import qualified Options.Applicative       as A
import           Options.Applicative.Types
import           System.IO
import           Text.HTML.TagSoup

import           Utils


type RecordInfo = (Int, Maybe T.Text, T.Text)
type FullRecord = (Int, Maybe T.Text, T.Text, Bool)


sql :: Query
sql =  "SELECT id, title, body FROM "
    <> recordsTable
    <> " WHERE body LIKE '%jpg%' OR body LIKE '%png%';"

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

pingURL' :: RecordInfo -> IO FullRecord
pingURL' (a, b, url) = (a, b, url,) <$> pingURL url


toMapRows :: (B.ByteString, B.ByteString, B.ByteString, B.ByteString)
          -> [FullRecord]
          -> [MapRow B.ByteString]
toMapRows (f1, f2, f3, f4) = (hrow :) . map row
    where
        hrow = M.fromList [ (f1, f1)
                          , (f2, f2)
                          , (f3, f3)
                          , (f4, f4)
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
main = do
    output <- execParser opt
    getBodies >>= mapM pingURL' . L.concatMap (spreadLast . second getImages)
              >>= writeOutput output . toMapRows header


opt' :: Parser FilePath
opt' = argument str (  metavar "OUTPUT"
                    <> help "The output file to write to.")

opt :: ParserInfo FilePath
opt = info (helper <*> opt')
           (  fullDesc
           <> progDesc "Find IMGs in Neatline items' bodies and test them."
           <> A.header "get-links -- Find IMGs in Neatline items' bodies and test them.")
