module Main where

import Database
import Controller
import Snap

main :: IO ()
main = do
  dbMigration
  quickHttpServe mainRouter