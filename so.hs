{-# LANGUAGE OverloadedStrings #-}

import Network.URI
import Network.HTTP
import Network.Curl
import Control.Monad.Maybe
import Control.Applicative
import Control.Monad.Trans
import Text.CSV
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment
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
constructSOQuestionsURL x = "http://api.stackoverflow.com/1.1/questions?answers=false&body=false&comments=false&fromdate=" ++ show x ++ "&tagged=scala&sort=creation"

toLeft :: Maybe a -> e -> Either e a 
toLeft ( Just x ) _ = Right x
toLeft Nothing e    = Left e

data Question = Question { title :: String , creationDate :: Integer} deriving Show
instance Json.FromJSON Question where
    parseJSON (Json.Object v) = Question <$> v Json..: "title" <*> v Json..: "creation_date"
    parseJSON _ = fail "malformed JSON response"

data Questions = Questions { qs :: [Question] } deriving Show
instance Json.FromJSON Questions where
    parseJSON (Json.Object v) = Questions <$> v Json..: "questions"
    parseJSON _ = fail "malformed JSON response"

toCSVRecord :: Question -> Record
toCSVRecord q = [show $ creationDate q,  title q]

toQ :: Record -> Question
toQ (rowId : rowTitle : _) = Question rowTitle (read rowId :: Integer)

toCSV :: Questions -> CSV
toCSV questions = map toCSVRecord $ qs questions

unZipResult :: String -> Either String C.ByteString
unZipResult x = Right $ GZip.decompress $ C.pack x

parseResult :: C.ByteString -> Either String CSV
parseResult x = fmap toCSV (Json.eitherDecode x :: Either String Questions)

diffCSVs :: Either a CSV -> Either b CSV -> Either String CSV
diffCSVs (Right csv1) (Right csv2) = let csv2Ids = fmap head csv2 in Right $ filter (\r -> head r `notElem` csv2Ids) csv1
diffCSVs _ _ = Left "Error"

mergeCSVs :: Either a CSV -> Either b CSV -> Either String CSV
mergeCSVs (Right csv1) (Right csv2) = let csv2Ids = fmap head csv2 in Right $ filter (\r -> head r `notElem` csv2Ids) csv1 ++ csv2
mergeCSVs _ _ = Left "Error"

sendToPebble :: String -> String -> Question -> IO ()
sendToPebble userToken appToken q = curlPost 
    "https://api.pushover.net/1/messages.json" 
    [ "token=" ++ appToken, 
        "user=" ++ userToken, 
        ("message=" ++ title q)]

showNewQs :: CSV -> (Question -> IO ()) -> IO ()
showNewQs qs sendQ =  do
    _ <- putStr $ printCSV qs
    _ <- sequence $ fmap (sendQ . toQ) (take 5 qs)
    return ()

main :: IO ()
main = do
    userToken : appToken :  _ <- getArgs
    currentTime <- getPOSIXTime
    let roundedCurrentTime = round currentTime
    let timeMillis = scalaQsInLast15MinsURL roundedCurrentTime
    let soURL = constructSOQuestionsURL timeMillis
    resultAsString <- runMaybeT $ openURL soURL
    let csvOfFetchedQs = do  
        result <- toLeft resultAsString "could not fetch teh URL"
        unzippedResult <- unZipResult result
        parseResult unzippedResult
    csvOfExistingQs <- parseCSVFromFile "so_questions_db"
    let csvOfNewQs = diffCSVs csvOfFetchedQs csvOfExistingQs
    let csvOfAllQs = mergeCSVs csvOfFetchedQs csvOfExistingQs
    _ <- writeFile "so_questions_db.new" $ printCSV (case csvOfAllQs of
        Left _ -> [[]]
        Right out -> out)
    _ <- removeFile "so_questions_db"
    _ <- renameFile "so_questions_db.new" "so_questions_db"
    case csvOfNewQs of 
        Right newQs -> showNewQs newQs $ sendToPebble userToken appToken
        Left err -> putStrLn err
