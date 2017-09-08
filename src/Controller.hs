{-# LANGUAGE OverloadedStrings #-}

module Controller (
  mainRouter
) where

import Database
import Model
import View
import Snap
import Data.Int
import Data.Text
import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy
import Data.Maybe
import Data.Aeson
import Control.Monad.IO.Class (liftIO, MonadIO)
import Database.Persist
import Database.Persist.Class

-- Here is a top level router
-- This will define the base and bookmarks routes

mainRouter :: Snap ()
mainRouter = route [
                  (     "", writeBS "") -- Base / route
                , ("bookmarks", bookmarksRouter) -- /bookmarks route
              ]

bookmarksRouter :: Snap ()
bookmarksRouter =  route [
                  (     "", method GET    bookmarksRouteIndex)
                , (     "", method POST   bookmarksRouteCreate)
                , (     "", method GET    bookmarksRouteShow)
                , (     "", method PUT    bookmarksRouteUpdate)
                , (     "", method DELETE bookmarksRouteDelete)
              ]

bookmarksRouteIndex :: Snap ()
bookmarksRouteIndex = do
  maybeLimitTo <- getParam "limit"
  maybeOffsetBy <- getParam "start"
  bookmarks <- liftIO $ getBookmarks maybeLimitTo maybeOffsetBy
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS $ encode $ Prelude.map entityIdToJSON bookmarks

bookmarksRouteShow :: Snap ()
bookmarksRouteShow = do
  set404AndContentType
  maybeBookmarkId <- getParam "id"
  (bookmarkIdKey, maybeBookmark) <- liftIO $ getBookmarkById maybeBookmarkId
  respondWithMaybeBookmark 200 bookmarkIdKey maybeBookmark

bookmarksRouteCreate :: Snap ()
bookmarksRouteCreate = do
  body <- readRequestBody 50000
  let bookmark = bookmarkJSONToBookmark $ parseBodyToBookmarkJSON body
  bookmarkIdKey <- liftIO $ insertBookmark bookmark
  modifyResponse $ setHeader "Content-Type" "application/json"
  respondWithBookmark 201 bookmarkIdKey bookmark
  
bookmarksRouteUpdate :: Snap ()
bookmarksRouteUpdate = do
  set404AndContentType
  maybeBookmarkId <- getParam "id"
  body <- readRequestBody 50000
  let bookmarkJSON = parseBodyToBookmarkJSON body
  (bookmarkIdKey, maybeBookmark) <- liftIO $ updateBookmarkById maybeBookmarkId bookmarkJSON
  respondWithMaybeBookmark 200 bookmarkIdKey maybeBookmark

bookmarksRouteDelete :: Snap ()
bookmarksRouteDelete = do
  set404AndContentType
  maybeBookmarkId <- getParam "id"
  (bookmarkIdKey, maybeBookmark) <- liftIO $ deleteBookmarkById maybeBookmarkId
  respondWithMaybeBookmark 200 bookmarkIdKey maybeBookmark

set404AndContentType :: Snap ()
set404AndContentType = do
  modifyResponse $ setResponseCode 404
  modifyResponse $ setHeader "Content-Type" "application/json"

parseBodyToBookmarkJSON :: Data.ByteString.Lazy.ByteString -> BookmarkJSON
parseBodyToBookmarkJSON body = fromMaybe (BookmarkJSON (Just "") (Just "")) (decode body :: Maybe BookmarkJSON)

respondWithMaybeBookmark :: Int -> Key Bookmark -> Maybe Bookmark -> Snap()
respondWithMaybeBookmark code bookmarkIdKey maybeBookmark = case maybeBookmark of
    -- Bookmark not found?
    Nothing -> writeBS ("{\"error\": \"Not found.\"}" :: Data.ByteString.ByteString)
    -- Bookmark found?
    -- The code is the HTTP status code
    Just bookmark -> respondWithBookmark code bookmarkIdKey bookmark
  
respondWithBookmark :: Int -> Key Bookmark -> Bookmark -> Snap()
respondWithBookmark code bookmarkIdKey bookmark = do
  modifyResponse $ setResponseCode code
  writeLBS $ bookmarkAsJSONLBS bookmarkIdKey bookmark
  