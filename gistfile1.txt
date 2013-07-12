{-# LANGUAGE OverloadedStrings #-}

import Network.URI
import Network.HTTP
import Control.Monad.Maybe
import Control.Applicative
import Control.Monad.Trans
import Data.Maybe
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Codec.Compression.GZip as GZip
import qualified Data.Aeson as Json

-- import qualified Data.ByteString.Lazy as ByteString
-- import qualified Codec.Compression.GZip as GZip

openURL :: String -> MaybeT IO String
openURL url = case parseURI url of
    Nothing -> fail ""
    Just u -> liftIO (getResponseBody =<< simpleHTTP (mkRequest GET u))


scalaQsInLast15MinsURL ::  Integer -> Integer
scalaQsInLast15MinsURL now = now - (15 * 60 * 1000) 

constructSOQuestionsURL :: Show a => a -> String
constructSOQuestionsURL x = "http://api.stackoverflow.com/1.1/questions?answers=false&body=false&comments=false&fromdate=" ++ ( show x ) ++ "&tagged=scala"
          

accumulateWords :: [a] -> a -> [a]
accumulateWords list word = word : list

unZipResult :: String -> (Either String C.ByteString)
unZipResult x = Right $ GZip.decompress $ C.pack x

byteStringToString :: C.ByteString -> String
byteStringToString x = reverse ( C.foldl accumulateWords [] x )

toLeft :: (Maybe a) -> e -> Either e a 
toLeft ( Just x ) _ = Right(x)
toLeft Nothing e    = Left e

showValues :: Questions -> [String]
showValues x = fmap show $ qs x

data Questions = Questions { qs :: [String] } deriving Show
instance Json.FromJSON Questions where
    parseJSON (Json.Object v) = Questions <$> v Json.: "questions"
    parseJSON _ = mzero

parseResult :: C.ByteString -> (Either String [String])
parseResult x = fmap showValues (Json.eitherDecode x :: Either String Questions)

main = do
    currentTime <- getPOSIXTime
    let timeMillis = scalaQsInLast15MinsURL $ round currentTime
    let soURL = constructSOQuestionsURL timeMillis
    maybeResult <- runMaybeT $ openURL soURL   
    let unzippedResult = fmap unZipResult maybeResult
    -- let parsedResult = fmap parseResult unzippedResult
    putStrLn $ (case unzippedResult of
        Nothing -> fail "could not fetch the URL: " ++ soURL
        Just (Left err) -> fail err 
        Just (Right s)  -> show $ parseResult s)
