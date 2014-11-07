{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Data.Int
import qualified Data.Text                 as T
import qualified Data.Text.Format          as F
import           Database.MySQL.Simple
import           Options.Applicative
import           Options.Applicative.Types

import           Utils


type UrlReplace = (T.Text, T.Text)


alterLinks :: UrlReplace -> IO Int64
alterLinks params = do
    cxn <- connect login
    execute cxn sql params <* close cxn
    where
        sql = "UPDATE " <> recordsTable <> " SET body=REPLACE(body, ?, ?);"


main :: IO ()
main =   execParser opt
     >>= alterLinks
     >>= F.print "Updated {} rows.\n" . F.Only


opt' :: Parser UrlReplace
opt' =   (,)
     <$> argument text (  metavar "ORIGINAL"
                       <> help "The original host name to replace.")
     <*> argument text (  metavar "REPLACEMENT"
                       <> help "The host name to replace into the database.")

opt :: ParserInfo UrlReplace
opt = info (helper <*> opt')
           (  fullDesc
           <> progDesc "Replace values in the Neatline items' bodies."
           <> header "alter-links -- replace values in the Neatline items' bodies.")

text :: ReadM T.Text
text = T.pack <$> readerAsk
