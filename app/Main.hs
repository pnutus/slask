{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Main where

import Web.Scotty as S
import Control.Monad.IO.Class
import TimeEdit
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A hiding (id)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Time.Format

default (T.Text)

main :: IO ()
main = scotty 8080 $ do
  get "/" frontPage
  get "/:room" $ do
    roomQuery <- S.param "room"
    table <- queryTable roomQuery
    blaze . htmlPage $ table

frontPageQueries :: [Text]
frontPageQueries = ["mvf", "mvl", "fl", "ha", "hb", "hc"]

frontPage :: ActionM ()
frontPage = do
  tables <- mapM queryTable frontPageQueries
  blaze . htmlPage . mconcat $ tables


queryTable :: Text -> ActionM Html
queryTable query = do
  resultEither <- liftIO $ roomStatusSearch query
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
  = (table ! A.style "display: inline-block; vertical-align: top; margin-right: 1em;")
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
