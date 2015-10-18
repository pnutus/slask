{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module TimeEdit where

import           Data.List
import           Data.Maybe
import           Data.Time
import           Data.Time.Format
import           Data.Time.Clock
import           Control.Monad.Reader

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy.Search (replace)

import qualified Data.Vector as V
import           Text.HTML.TagSoup
import           Data.Csv

import           Debug.Trace (trace)
import           Network.HTTP.Client.Conduit

default (Text)


debug = flip trace

-- TODO: get iCal link for courses

data RoomStatus
  = FreeUntil TimeOfDay
  | Occupied
  | Free
  deriving (Show)

data Room
  = Room Text RoomStatus
  deriving (Show)

roomStatus :: TimeOfDay -> SearchResults -> [Room]
roomStatus time = map toRoom
  where
    toRoom (name, lessons) = Room name status
      where
        status
          | any (isDuring time) lessons = Occupied
          | otherwise = maybe Free FreeUntil . find (> time)
                      . map startTime . sortOn startTime $ lessons

isDuring :: TimeOfDay -> Lesson -> Bool
isDuring time lesson = startTime lesson <= time && time <= endTime lesson

roomStatusSearch :: Text -> ReaderT Manager IO (Either Text [Room])
roomStatusSearch query = do
  currentTime <- liftIO $ localTimeOfDay <$> now
  search (roomStatus currentTime) RoomQuery query


scheduleToday :: Text -> ReaderT Manager IO ()
scheduleToday query = do
  roomResult <- search textFromSearch RoomQuery query
  case roomResult of
    Right result -> liftIO $ T.putStr result
    Left _ -> do
      courseResult <- search textFromSearch CourseQuery query
      case courseResult of
        Right result -> liftIO $ T.putStr result
        Left _ -> liftIO $ T.putStrLn $ T.concat [
          "No room or course found for \"", query, "\""]

-- * Searching timeEdit

type SearchResults = [(TimeEditName, [Lesson])]

search
  :: (SearchResults -> a)
  -> QueryType -> Text
  -> ReaderT Manager IO (Either Text a)
search f searchType query = do
  manager <- newManager
  html <- httpGet $ searchUrl searchType query
  case parseTimeEditSearchResults $ textFromLazyBS html of
    [] -> return . Left $ T.concat
        ["No ", T.toLower (tshow searchType), " found for \"", query, "\""]
    nameids -> Right . f
      <$> (mapM.mapM) downloadTodaysSchedule nameids

textFromSearch :: SearchResults -> Text
textFromSearch = T.unlines . map (uncurry prettySchedule)

downloadTodaysSchedule
  :: TimeEditId
  -> ReaderT Manager IO [Lesson]
downloadTodaysSchedule id = do
  today <- liftIO $ localDay <$> now
  csv <- httpGet $ scheduleUrl today today [id]
  return $ parseCsv $ replace ", " ("," :: BS.ByteString) csv

-- TODO: prettySchedule for different search types

now :: IO LocalTime
now = do
  timeZone <- getCurrentTimeZone
  utcToLocalTime timeZone <$> getCurrentTime
  parseTimeM True defaultTimeLocale "%y%m%d %H:%M" "151014 10:30"

prettySchedule :: TimeEditName -> [Lesson] -> Text
prettySchedule name lessons
  = T.unlines (name : schedule lessons)
  where
    schedule []      = ["-"]
    schedule lessons = map prettyTimes lessons
    format = T.pack . formatTime defaultTimeLocale "%H:%M"
    prettyTimes lesson = T.intercalate "–"
      $ map (\f -> format $ f lesson) [startTime, endTime]

-- * Convenience

httpGet :: Text -> ReaderT Manager IO L.ByteString
httpGet url = do
  request <- parseUrl $ T.unpack url
  responseBody <$> httpLbs request

textFromLazyBS :: L.ByteString -> Text
textFromLazyBS = E.decodeUtf8

tshow :: Show a => a -> Text
tshow = T.pack . show



-- * Search types

data QueryType
  = RoomQuery
  | CourseQuery

instance Show QueryType where
  show RoomQuery = "Room"
  show CourseQuery = "Course"

stringFromSearchType :: QueryType -> Text
stringFromSearchType searchType
  = case searchType of
    RoomQuery -> "186"
    CourseQuery -> "182"



-- * Html Scraping

type TimeEditId = Text
type TimeEditName = Text

parseTimeEditSearchResults :: Text -> [(TimeEditName, TimeEditId)]
parseTimeEditSearchResults
  = map extract
  . filter (~== TagOpen "div" [(attrId, ""), (attrName, "")])
  . parseTags
  where
    extract div = (fromAttrib attrName div, fromAttrib attrId div)
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
    = pure . Rooms . map E.decodeUtf8 . L.words . L.fromStrict

parseCsv :: L.ByteString -> [Lesson]
parseCsv csv = V.toList vector
  where
    vector = either error id (decode HasHeader cleanCsv)
    cleanCsv = dropLines 3 csv

dropLines :: Int -> L.ByteString -> L.ByteString
dropLines n = L.unlines . drop n . L.lines

-- * urls

-- TODO remove unnecessary stuff from urls

baseUrl :: Text
baseUrl = "https://se.timeedit.net/web/chalmers/db1/public/"

searchUrl :: QueryType -> Text -> Text
searchUrl searchType query = T.concat [
  baseUrl, "objects.html?fr=t&partajax=t&im=f&sid=1004&l=sv",
  "&search_text=", query, "&types=", typeString]
  where
    typeString = stringFromSearchType searchType

scheduleUrl :: Day -> Day -> [TimeEditId] -> Text
scheduleUrl startTime endTime ids = T.concat [
  baseUrl, "ri.csv?sid=3&l=en_US",
  "&p=", format startTime, "-", format endTime,
  "&objects=", T.intercalate "," ids]
  where
    format = T.pack . formatTime defaultTimeLocale "%y%m%d"
