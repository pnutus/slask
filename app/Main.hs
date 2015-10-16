module Main where

import TimeEdit

main :: IO ()
main = do
  putStrLn "Enter room name"
  query <- getLine
  search Course query
