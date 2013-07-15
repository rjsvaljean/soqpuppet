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

openURL :: String -> MaybeT IO String
openURL url = case parseURI url of
    Nothing -> fail ""
    Just u -> liftIO (getResponseBody =<< simpleHTTP (mkRequest GET u))

scalaQsInLast15MinsURL ::  Integer -> Integer
scalaQsInLast15MinsURL now = now - (15 * 60 * 1000) 

constructSOQuestionsURL :: Show a => a -> String
constructSOQuestionsURL x = "http://api.stackoverflow.com/1.1/questions?answers=false&body=false&comments=false&fromdate=" ++ ( show x ) ++ "&tagged=scala&sort=creation"

accumulateWords :: [a] -> a -> [a]
accumulateWords list word = word : list

byteStringToString :: C.ByteString -> String
byteStringToString x = reverse ( C.foldl accumulateWords [] x )

toLeft :: (Maybe a) -> e -> Either e a 
toLeft ( Just x ) _ = Right(x)
toLeft Nothing e    = Left e

data Question = Question { title :: String } deriving Show
instance Json.FromJSON Question where
    parseJSON (Json.Object v) = Question <$> v Json..: "title"
    parseJSON _ = fail "malformed JSON response"

data Questions = Questions { qs :: [Question] } deriving Show
instance Json.FromJSON Questions where
    parseJSON (Json.Object v) = Questions <$> v Json..: "questions"
    parseJSON _ = fail "malformed JSON response"

showQuestions :: Questions -> [String]
showQuestions questions = map (\q -> title q) $ qs questions

unZipResult :: String -> (Either String C.ByteString)
unZipResult x = Right $ GZip.decompress $ C.pack x

parseResult :: C.ByteString -> (Either String [String])
parseResult x = fmap showQuestions (Json.eitherDecode x :: Either String Questions)

main = do
    currentTime <- getPOSIXTime
    let roundedCurrentTime = round currentTime
    let timeMillis = scalaQsInLast15MinsURL roundedCurrentTime
    let soURL = constructSOQuestionsURL timeMillis
    resultAsString <- runMaybeT $ openURL soURL
    let unzippedParsedResult = do  
        result <- (toLeft resultAsString "could not fetch teh URL")
        unzippedResult <- unZipResult result
        parsedResult <- parseResult unzippedResult
        return parsedResult
    putStrLn $ (case unzippedParsedResult of
        Left err -> show err 
        Right s  -> "Questions as of " ++ (show roundedCurrentTime) ++ " \n" ++ (show s))
