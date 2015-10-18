{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Main where

import Web.Scotty as S
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Monoid
import TimeEdit
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A hiding (id)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Time.Format
import Network.HTTP.Client.Conduit

default (T.Text)

main :: IO ()
main = do
  manager <- newManager
  scotty 8080 $ do
    get "/" $ do
      maybeQuery <- processParams <$> S.params
      case maybeQuery of
        Nothing -> frontPage manager ""
        Just roomQuery -> do
          table <- queryTable manager roomQuery
          blaze . template $ searchBox roomQuery <> table
    notFound $
      frontPage manager ""

processParams :: [Param] -> Maybe Text
processParams (("room", query):_) = case query of
  "" -> Nothing
  q  -> Just q
processParams _ = Nothing

frontPageQueries :: [Text]
frontPageQueries = ["mvf", "mvl", "fl", "ha", "hb", "hc"]

frontPage :: Manager -> Text -> ActionM ()
frontPage manager query = do
  tables <- mapM (queryTable manager) frontPageQueries
  blaze . template . mconcat $ searchBox query : tables


queryTable :: Manager -> Text -> ActionM Html
queryTable manager query = do
  resultEither <- liftIO $ runReaderT (roomStatusSearch query) manager
  return . toHtml . htmlResults $ resultEither

htmlResults :: Either Text [Room] -> Html
htmlResults = either (p . toHtml) roomStatusTable

searchBox :: Text -> Html
searchBox query
  = H.div $
      H.form ! A.method "get" $
        input ! A.name "room"
              ! A.type_ "text"
              ! A.placeholder "room name"
              ! A.autofocus ""
              ! A.value (lazyTextValue query)

template :: Html -> Html
template pageBody
  = docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.style css
    H.body pageBody

css :: Html
css =
  "body { font-size: 30px; font-family: Helvetica, sans-serif; }\
  \table { display: inline-block; vertical-align: top; margin: 0.5em; }\
  \form { text-align: center; }\
  \input { font-size: 50px; }\
  \.green { color: green; }\
  \.red   { color: red; }"

green :: Html -> Html
green = H.span ! A.class_ "green"

red :: Html -> Html
red = H.span ! A.class_ "red"

roomStatusTable :: [Room] -> Html
roomStatusTable = table . mapM_ htmlFromRoom

htmlFromRoom :: Room -> Html
htmlFromRoom (Room name status)
  = tr $ do
    td $ toHtml name
    td $ htmlFromRoomStatus status

htmlFromRoomStatus :: RoomStatus -> Html
htmlFromRoomStatus status = case status of
  Occupied -> red "Occupied"
  Free -> green "Free today"
  (FreeUntil time)
    -> green . toHtml $ "Free until " ++ format time
  where
    format = formatTime defaultTimeLocale "%H:%M"

blaze = S.html . renderHtml
