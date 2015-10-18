{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Main where

import Web.Scotty as S
import Control.Monad.IO.Class
import Control.Monad.Reader
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
      frontPage manager
    get "/:room" $ do
      roomQuery <- S.param "room"
      table <- queryTable manager roomQuery
      blaze . htmlPage $ table

frontPageQueries :: [Text]
frontPageQueries = ["mvf", "mvl", "fl", "ha", "hb", "hc"]

frontPage :: Manager -> ActionM ()
frontPage manager = do
  tables <- mapM (queryTable manager) frontPageQueries
  blaze . htmlPage . mconcat $ tables


queryTable :: Manager -> Text -> ActionM Html
queryTable manager query = do
  resultEither <- liftIO $ runReaderT (roomStatusSearch query) manager
  return . toHtml . htmlResults $ resultEither

htmlResults :: Either Text [Room] -> Html
htmlResults = either (p . toHtml) roomStatusTable

htmlPage :: Html -> Html
htmlPage pageBody
  = docTypeHtml
  . (H.body ! A.style "font-size: 20pt; font-family: Helvetica, sans-serif;")
  $ pageBody

roomStatusTable :: [Room] -> Html
roomStatusTable
  = (table ! A.style "display: inline-block; vertical-align: top; margin: 0.5em;")
  . mapM_ htmlFromRoom

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
    -> green $ "Free until " ++ format time
  where
    format = formatTime defaultTimeLocale "%H:%M"
    colorSpan color
      = (H.span ! A.style (stringValue $ concat ["color: ", color, ";"]))
      . toHtml
    green = colorSpan "green"
    red   = colorSpan "red"

blaze = S.html . renderHtml
