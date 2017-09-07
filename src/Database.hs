{-# LANGUAGE OverloadedStrings #-}

module Database (
    dbMigration
  , getBookmarks
  , getBookmarkByIdbook
  , insertBookmark
  , updateBookmarkById
  , deleteBookmarkById
) where

-- Here we import our custom modules: Model and View
-- These are defined in src/Model.hs and src/View.hs

import Model
import View

-- These are our build dependencies

import System.Environment -- To get the DB connection string

-- Deals with strings and integers

import Data.Int
import Data.Text
import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy

-- Needed for 'fromMaybe'

import Data.Aeson

-- Used in the 'withDbRun' type signature

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control
import Control.Monad.Logger

-- Nedeed for interfacing with SQLite

import Database.Persist
import Database.Persist.Class
import Database.Persist.Sqlite as DbSql

-- Gather the database connection string from the env
-- If not set use the default

sqliteConnString :: IO Data.Text.Text
sqliteConnString = do
  maybeDbConnString <- lookupEnv "WEB_BOOKMARKS_DB_CONN"
  return $ Data.Text.pack $ fromMaybe "webBookmarks_default.db" maybeDbConnString

-- Needed for each database transaction (inserting, updating, retrieval, deleting)

withDbRun :: SqlPersistT (NoLogginT (resourceT IO)) b -> IO b
withDbRun command = do
  connString <- sqliteConnString
  runSqlite connString command

-- This will create our web bookmarks table if it does not already exist
-- Perist will assist with update our table schema should our model change

dbMigration :: IO ()
dbMigration = withDbRun $ runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe bookmark)

-- Helper function to convert the URL ID string to the needed 64 bit integer primary key

getBookmarksIdKey :: Maybe Data.ByteString.ByteString -> Key bookmark
getBookmarksIdKey maybeIdBS = toSqlKey bookmarkIdInt64
  where
    -- If we receive `Nothing` for the ID, we will return an invalid ID of `-1`
    bookmarkIdBD = fromMaybe ("-1" :: Data.ByString.ByteString) maybeIdBS
    -- Convert the string the needed 64 bit integer
    bookmarkIdInt64 = read (Data.ByteString.Char8.unpack bookmarkIdBS) :: Int64

-- Retrieves multiple bookmark rows from our table starting at `start` and up to the `limit`

getBookmarks :: Maybe Data.ByteString.ByteString -> Maybe Data.ByteString.ByteString -> IO [Entity Bookmark]
getBookmarks maybeLimitTo maybeOffsetBy = do
  -- If the limit and offset are `Nothing`, we will use the defaults 10 for the limit and 0 for the offset
  let limitToBS = fromMaybe ("10" :: Data.ByteString.ByteString) maybeLimitTo
  let offsetByBS = fromMaybe ("0" :: Data.ByteString.ByteString) maybeOffsetBy
  -- Converts the strings to integers
  let limitToInt = read (Data.ByteString.Char8.unpack limitToBS) :: Int
  let offsetByInt = read (Data.ByteString.Char8.unpack offsetByBS) :: Int
  -- The actual database call
  withDbRun $ DbSql.selectList ([] :: [Filter Bookmark]) [LimitTo limitToInt, OffsetBy offsetByInt]

  getBookmarkByIdbook :: Maybe Data.ByteString.ByteString -> IO (Key Bookmark, Maybe Bookmark)
  getBookmarkByIdbook maybeIdBS = do
    -- Get the bookmark primary key
    let bookmarkIdKey = getBookmarksIdKey maybeIdBS
    -- Retrieve the bookmark from the database
    maybeBookmark <- withDbRun $ DbSql.get bookmarkIdKey
    -- Return both the primary key and maybe the bookmark (if it actually exists in the database)
    return (bookmarkIdKey, maybeBookmark)

  insertBookmark :: Bookmark -> IO (Key Bookmark)
  -- Create a new bookmark row in the database
  insertBookmark bookmark = withDbRun $ DbSql.insert bookmark

  updateBookmarkById :: Maybe Data.ByteString.ByteString -> BookmarkJSON -> IO (Key Bookmark, Maybe Bookmark)
  updateBookmarkById maybeIdBS bookmarkJSON = do
    let bookmarkIdKey = getBookmarksIdKey maybeIdBS
    -- Look up the bookmark in the database
    (bookmarkIdKeyId, maybeBookmark) <- getBookmarkByIdbook maybeIdBS
    case maybeBookmark of
      -- If the book mark does not exist, return `Nothing`
      Nothing -> return (bookmarkIdKeyId, Nothing)
      -- If the book mark does exist
      Just bookmark -> do
        -- Create an updated bookmark record
        let bookmarkUpdated = Bookmark {
            -- The JSON maybe not have the title so use the bookmark's current title
            bookmarkTitle = fromMaybe (bookmarkTitle bookmark) (bookmarkJSONTitle bookmarkJSON)
            -- The JSON maybe not have the URL so use the bookmark's current URL
          , bookmarkUrl = fromMaybe (bookmarkUrl bookmark) (bookmarkJSONUrl bookmarkJSON)
        }
        -- Update the bookmark's title and URL in the database
        withDbRun $ DbSql.update bookmarkKeyId [
              BookmarkTitle =. BookmarkTitle bookmarkUpdated
            , bookmarkUrl =. bookmarkUrl bookmarkUpdated
          ]
        return (bookmarkKeyId, Just bookmarkUpdated)

    deleteBookmarkById :: Maybe Data.ByteString.ByteString -> IO (Key Bookmark, Maybe Bookmark)
    deleteBookmarkById maybeIdBS = do
      let bookmarkIdKey = getBookmarksIdKey maybeIdBS
      -- Look up the bookmark in the database
      (bookmarkKeyId, maybeBookmark) <- getBookmarkById maybeIdBS
      case maybeBookmark of
        -- No bookmark?
        Nothing -> return (bookmarkKeyId, Nothing)
        -- Bookmark?
        Just bookmark -> do
          -- Delete the bookmark from the database
          withDbRun $ DbSql.delete bookmarkIdKeyId
          return (bookmarkIdKey, Just bookmark)
      
