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

import           Network.HTTP.Conduit
import           Text.HTML.TagSoup
import           Data.Csv

default (Text)

scheduleToday :: String -> IO ()
scheduleToday = search Course

search :: SearchType -> String -> IO ()
search searchType query = do
  manager <- newManager tlsManagerSettings
  html <- httpGet manager $ searchUrl searchType textQuery
  case parseTimeEditResults (textFromLazyBS html) of
    [] -> T.putStrLn errorString
    xs -> mapM_ (prettySchedule manager) xs
  where
    textQuery = T.pack query
    errorString = T.concat
      ["No ", T.toLower (tshow searchType), " found for \"", textQuery, "\""]

prettySchedule :: Manager -> (TimeEditId, TimeEditName) -> IO ()
prettySchedule manager (id, name) = do
      today <- utctDay <$> getCurrentTime
      csv <- httpGet manager $ scheduleUrl today today [id]
      T.putStrLn $ textFromLazyBS csv


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

-- * Scraping

maybeAttrib tag attr
  | isTagOpen tag = Just $ fromAttrib attr tag
  | otherwise     = Nothing

type TimeEditId = Text
type TimeEditName = Text

parseTimeEditResults :: Text -> [(TimeEditId, TimeEditName)]
parseTimeEditResults
  = mapMaybe processResult
  . filter (~== TagOpen "div" [("data-id", ""), ("data-name", "")])
  . parseTags
  where
    processResult div
      = (,) <$> maybeAttrib div "data-id"
            <*> maybeAttrib div "data-name"

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
