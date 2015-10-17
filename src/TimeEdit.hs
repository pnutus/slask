{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module TimeEdit where

import           Data.List
import           Data.Maybe
import           Data.Time
import           Data.Time.Format
import           Data.Time.Clock

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as BS

import qualified Data.Vector as V
import           Network.HTTP.Conduit
import           Text.HTML.TagSoup
import           Data.Csv

default (Text)

scheduleToday :: String -> IO ()
scheduleToday query = do
  roomResult <- search Room query
  case roomResult of
    Right result -> T.putStr result
    Left _ -> do
      courseResult <- search Course query
      case courseResult of
        Right result -> T.putStr result
        Left _ -> T.putStr $ T.concat [
          "No room or course found for \"", T.pack query, "\""]

search :: SearchType -> String -> IO (Either Text Text)
search searchType query = do
  manager <- newManager tlsManagerSettings
  html <- httpGet manager $ searchUrl searchType textQuery
  case parseTimeEditResults (textFromLazyBS html) of
    [] -> return $ Left errorString
    xs -> (Right . T.unlines) <$> mapM (prettySchedule manager) xs
  where
    textQuery = T.pack query
    errorString = T.concat
      ["No ", T.toLower (tshow searchType), " found for \"", textQuery, "\""]

prettySchedule :: Manager -> (TimeEditId, TimeEditName) -> IO Text
prettySchedule manager (id, name) = do
  today <- utctDay <$> getCurrentTime
  csv <- httpGet manager $ scheduleUrl today today [id]
  return $ T.unlines (name : schedule (parseCsv csv))
  where
    schedule []      = ["-"]
    schedule lessons = map prettyTimes lessons
    format = T.pack . formatTime defaultTimeLocale "%H:%M"
    prettyTimes lesson = T.intercalate "–"
      $ map (\f -> format $ f lesson) [startTime, endTime]




-- * Convenience

httpGet :: Manager -> Text -> IO L.ByteString
httpGet manager url = do
  request <- parseUrl $ T.unpack url
  responseBody <$> httpLbs request manager

textFromLazyBS :: L.ByteString -> Text
textFromLazyBS = E.decodeUtf8 . L.toStrict

tshow :: Show a => a -> Text
tshow = T.pack . show



-- * Search types

data SearchType
  = Room
  | Course
  deriving (Show)

stringFromSearchType :: SearchType -> Text
stringFromSearchType searchType
  = case searchType of
    Room -> "186"
    Course -> "182"



-- * Html Scraping

type TimeEditId = Text
type TimeEditName = Text

parseTimeEditResults :: Text -> [(TimeEditId, TimeEditName)]
parseTimeEditResults
  = map extract
  . filter (~== TagOpen "div" [(attrId, ""), (attrName, "")])
  . parseTags
  where
    extract div = (fromAttrib attrId div, fromAttrib attrName div)
    attrId = "data-id"
    attrName = "data-name"




-- * CSV scraping

data Lesson
  = Lesson
  { startDate  :: Day
  , startTime  :: TimeOfDay
  , endDate    :: Day
  , endTime    :: TimeOfDay
  , courseName :: Maybe Text
  , rooms      :: Rooms
  }
  deriving (Show)

data Rooms
  = Rooms [Text]
  deriving (Show)

instance FromRecord Lesson where
  parseRecord v
    = Lesson <$> v .! 0
             <*> v .! 1
             <*> v .! 2
             <*> v .! 3
             <*> v .! 4
             <*> v .! 5

instance FromField Day where
  parseField
    = pure . parseTimeOrError True defaultTimeLocale "%Y-%m-%d"
    . BS.unpack

instance FromField TimeOfDay where
  parseField
    = pure . parseTimeOrError True defaultTimeLocale "%H:%M"
    . BS.unpack

instance FromField Rooms where
  parseField
    = pure . Rooms . map E.decodeUtf8 . BS.words

parseCsv :: L.ByteString -> [Lesson]
parseCsv csv = V.toList vector
  where
    vector = either error id (decode HasHeader cleanCsv)
    cleanCsv = dropLines 3 csv

dropLines :: Int -> L.ByteString -> L.ByteString
dropLines n = L.unlines . drop n . L.lines

-- * urls

baseUrl :: Text
baseUrl = "https://se.timeedit.net/web/chalmers/db1/public/"

searchUrl :: SearchType -> Text -> Text
searchUrl searchType query = T.concat [
  baseUrl, "objects.html?fr=t&partajax=t&im=f&sid=1004&l=sv",
  "&search_text=", query, "&types=", typeString]
  where
    typeString = stringFromSearchType searchType

scheduleUrl :: Day -> Day -> [TimeEditId] -> Text
scheduleUrl startTime endTime ids = T.concat [
  baseUrl, "ri.csv?sid=3",
  "&p=", format startTime, "-", format endTime,
  "&objects=", T.intercalate "," ids]
  where
    format = T.pack . formatTime defaultTimeLocale "%y%m%d"
