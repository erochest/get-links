{-# LANGUAGE OverloadedStrings #-}


module Utils
    ( login
    , pingURL
    , recordsTable
    ) where


import           Control.Applicative
import           Control.Exception
import           Control.Lens
import qualified Data.Text             as T
import           Database.MySQL.Simple
import           Network.Wreq          hiding (header)


recordsTable :: Query
recordsTable = "omeka_neatline_records"

login :: ConnectInfo
login = defaultConnectInfo { connectDatabase = "intersections" }


pingURL :: T.Text -> IO Bool
pingURL url = do
    putStr $ "Testing " ++ url' ++ " ... "
    status <- catch ((^. responseStatus. statusCode) <$> get url')
                    ((const $ return 404) :: SomeException -> IO Int)
    putStrLn $ "status = " ++ show status
    return $ status == 200
    where
        url' = T.unpack url
