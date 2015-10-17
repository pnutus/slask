{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty as S
import Control.Monad.IO.Class
import TimeEdit
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

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
      mapM_ htmlFromRoom rooms

htmlFromRoom :: Room -> Html
htmlFromRoom (Room name status)
  = do
    p $ b $ toHtml name
    p $ toHtml $ show status
