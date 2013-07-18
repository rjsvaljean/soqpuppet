{-# LANGUAGE OverloadedStrings #-}

import Network.URI
import Network.HTTP
import Control.Monad.Maybe
import Control.Applicative
import Control.Monad.Trans
import Text.CSV
import Data.Time.Clock.POSIX
import System.IO
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

putLine :: Handle -> String -> IO String
putLine handle line = do 
    _ <- hPutStrLn handle line
    return line

toOnlyNewQs :: [String] -> [Record] -> [Record]
toOnlyNewQs existingQIDs = filter (\q -> head q `elem` existingQIDs)

diffCSVs :: Either a CSV -> Either b CSV -> Either String CSV
diffCSVs (Right csv1) (Right csv2) = let csv2Ids = fmap head csv2 in Right $ filter (\r -> not $ head r `elem` csv2Ids) csv1
diffCSVs _ _ = Left "Error"

main :: IO String
main = do
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
    _ <- appendFile "so_questions_db" $ printCSV (case csvOfNewQs of
        Left err -> [[]]
        Right out -> out)
    return $ case csvOfNewQs of 
        Right newQs -> printCSV newQs
        Left err -> err

    

--     existingQsCSV <- fmap toQ $ parseCSVFromFile "so_questions_db"
--     let newQs
--     newQs <- fmap (toOnlyNewQs existingQIDs) unzippedParsedResult
--     

--     newQs <- withFile "so_questions_db" WriteMode (\handle -> do
--         -- contents <- hGetContents handle
--         case unzippedParsedResult of
--             Left err         -> return ["Error: " ++ err]
--             Right questions  -> let newQuestions = filter (\i -> True) questions in sequence $ fmap (putLine handle) newQuestions) 
--     putStrLn $ "Questions as of " ++ (show roundedCurrentTime) ++ ":\n" ++ (show newQs)
