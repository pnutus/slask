{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Main where

import Web.Scotty as S
import Control.Monad.IO.Class
import TimeEdit
import qualified Data.Text.Lazy as T
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Time.Format

default (T.Text)

main :: IO ()
main = scotty 8080 $ do
  get "/" $ S.html "Enter a room name in the url"
  get "/:room" $ do
    roomQuery <- S.param "room"
    resultEither <- liftIO $ roomStatusSearch roomQuery
    either S.html (S.html . renderHtml . htmlPage) resultEither

htmlPage :: [Room] -> Html
htmlPage rooms
  = docTypeHtml $ do
    H.body $ do
      table $ mapM_ htmlFromRoom rooms

htmlFromRoom :: Room -> Html
htmlFromRoom (Room name status)
  = tr $ do
    td $ b $ toHtml name
    td $ htmlFromRoomStatus status

htmlFromRoomStatus :: RoomStatus -> Html
htmlFromRoomStatus status = case status of
  Busy -> red $ toHtml "Busy"
  Free -> green $ toHtml "Free"
  (FreeUntil time)
    -> green $ toHtml $ "Free until " ++ format time
  where
    format = formatTime defaultTimeLocale "%H:%M"
    colorSpan color
      = (H.span ! A.style (stringValue $ concat ["color: ", color, ";"])) . toHtml
    green = colorSpan "green"
    red   = colorSpan "red"
