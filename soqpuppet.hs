{-# LANGUAGE OverloadedStrings #-}

import Network.URI
import Network.HTTP
import Network.Curl
import Control.Monad.Maybe
import Control.Applicative
import Control.Arrow
import Control.Monad.Trans
import Text.CSV
import Text.ParserCombinators.Parsec.Error
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Codec.Compression.GZip as GZip
import qualified Data.Aeson as Json

dbFileName :: String
dbFileName = "/.soq_db"

soqDb :: String -> String
soqDb prefix = prefix ++ dbFileName

openURL :: String -> MaybeT IO String
openURL url = case parseURI url of
    Nothing -> fail ""
    Just u -> liftIO (getResponseBody =<< simpleHTTP (mkRequest GET u))

millis15SecsBack ::  POSIXTime -> Integer
millis15SecsBack now = (round now) - (15 * 60 * 1000) 

constructSOQuestionsURL :: Show a => String -> a -> String
constructSOQuestionsURL tag timeSince = "http://api.stackoverflow.com/1.1/questions?" ++ 
    "answers=false" ++ 
    "&body=false" ++ 
    "&comments=false" ++ 
    "&fromdate=" ++ show timeSince ++ 
    "&tagged=" ++ tag ++ 
    "&sort=creation"

soqURL :: Integer -> String
soqURL qid = "http://stackoverflow.com/questions/" ++ (show qid)

postToPushover :: [String] -> IO()
postToPushover = curlPost "https://api.pushover.net/1/messages.json" 

toLeft :: Maybe a -> e -> Either e a 
toLeft ( Just x ) _ = Right x
toLeft Nothing e    = Left e

data Question = Question { title :: String , question_id :: Integer} deriving Show
instance Json.FromJSON Question where
    parseJSON (Json.Object v) = Question <$> v Json..: "title" <*> v Json..: "question_id"
    parseJSON _ = fail "malformed JSON response"

data Questions = Questions { qs :: [Question] } deriving Show
instance Json.FromJSON Questions where
    parseJSON (Json.Object v) = Questions <$> v Json..: "questions"
    parseJSON _ = fail "malformed JSON response"

toCSVRecord :: Question -> Record
toCSVRecord q = [show $ question_id q,  title q]

toQ :: Record -> Question
toQ (rowId : rowTitle : _) = Question rowTitle (read rowId :: Integer)

toCSV :: Questions -> CSV
toCSV questions = map toCSVRecord $ qs questions

unZipResult :: String -> Either String C.ByteString
unZipResult x = Right $ GZip.decompress $ C.pack x

parseResult :: C.ByteString -> Either String CSV
parseResult x = fmap toCSV (Json.eitherDecode x :: Either String Questions)

csvOp :: (CSV -> CSV -> CSV) -> Either String CSV -> Either ParseError CSV -> Either String CSV
csvOp f a b = (pure f) <*> a <*> (left show b)

diffCSVs' :: CSV -> CSV -> CSV
diffCSVs' csv1 csv2 = let csv2Ids = fmap head csv2 
    in filter (\r -> head r `notElem` csv2Ids) csv1

mergeCSVs' :: CSV -> CSV -> CSV
mergeCSVs' csv1 csv2 = let csv2Ids = fmap head csv2 
    in filter (\r -> head r `notElem` csv2Ids) csv1 ++ csv2

sendToPebble :: String -> String -> CSV -> IO [()]
sendToPebble userToken appToken qs = let 
    paramsForQs question = [ 
        "token=" ++ appToken, 
        "user=" ++ userToken, 
        "message=" ++ title question,
        "url=" ++ soqURL (question_id question)] 
    in sequence $ fmap (postToPushover . paramsForQs . toQ) (take 5 qs)

showNewQs :: CSV  -> IO ()
showNewQs qs =  putStr $ printCSV qs

persistAllQuestions :: String -> CSV -> IO ()
persistAllQuestions db csvOfAllQs = do
    _ <- writeFile (db ++ ".new") $ printCSV csvOfAllQs
    _ <- removeFile db
    renameFile (db ++ ".new") db

fetchedQsAsCSV :: Maybe String -> String -> Either String CSV
fetchedQsAsCSV resultAsString soURL = do  
    result <- toLeft resultAsString ("Could not fetch URL: " ++ soURL)
    unzippedResult <- unZipResult result
    parseResult unzippedResult

validCommandLineArgsHandler :: (String, String, String) -> IO ()
validCommandLineArgsHandler (userToken, appToken, tag) = do
    currentTime <- getPOSIXTime
    home <- getHomeDirectory
    let soURL = constructSOQuestionsURL tag $ millis15SecsBack currentTime
    resultAsString <- runMaybeT $ openURL soURL
    let csvOfFetchedQs = fetchedQsAsCSV resultAsString soURL
    csvOfExistingQs <- parseCSVFromFile $ soqDb home
    let csvOfNewQs = (csvOp diffCSVs') csvOfFetchedQs csvOfExistingQs
    let csvOfAllQs = (csvOp mergeCSVs') csvOfFetchedQs csvOfExistingQs
    _ <- case fmap (persistAllQuestions $ soqDb home) csvOfAllQs of
        Left err -> putStrLn err
        Right write -> do _ <- write; putStrLn "Persisted the new Questions"
    case csvOfNewQs of 
        Right newQs -> do 
            _ <- showNewQs newQs
            _ <- sendToPebble userToken appToken newQs
            putStrLn "Sent new Questions to Pushover"
        Left err -> putStrLn err

main :: IO ()
main = do
    args <- getArgs
    case args of 
        userToken : appToken : tag : _ -> validCommandLineArgsHandler (userToken, appToken, tag)
        _                              -> putStrLn "Usage: soqpuppet <userToken> <appToken> <tag>"
