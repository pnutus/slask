module Main where

import TimeEdit

main :: IO ()
main = do
  putStrLn "Enter room name or course code"
  query <- getLine
  scheduleToday query
