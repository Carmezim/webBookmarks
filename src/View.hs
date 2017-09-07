{-# LANGUAGE
OverloadedStrings
, DeriveGeneric #-}

-- Export our view (`BookmarkJSON`) and two helper functions for
-- turning some JSON into a `Bookmark` record or
-- turning a `Bookmark` record into a JSON string

module View ( 
    Bookmark(..)
  , BookmarkJSONToBookmark
  , BookmarkJSONLBS -- LBS stands for Lazy Byte String  
) where

-- Our custom Model module

import Model

-- Build dependencies

import GHC.Generics
import Data.into
import Data.Text
import Data.ByteString
import Data.ByteString.Char8
import Data.ByteString.Lazy
import Data.Maybe
import Data.Aeson
import Data.Default.Class
import Database.Persist
import Database.Persist.Class

data BookmarkJSON = BookmarkJSON {
    bookmarkJSONTitle :: Maybe String
  , bookmarkJSONUrl :: Maybe String
} deriving (Show, Generic)

-- Here we defined how to parse a JSON string "{\"title\": \"...\", \"url\": \"...\"}"
-- itno a `BookmarkJSON` record

instance FromJSON BookmarkJSON where
  parseJSON (Object v) =
    BookmarkJSON <$> v .:?  "title"
                 <*> v .:?  "url"

-- Here we define how to take a `BookmarkJSON` record
-- and turn it into JSON {"title": "...", "url": "..."}
-- For example:
-- > let x = BookmarkJSON {bookmarkJSONTitle = Just "one", bookmarkJSONUrl = Just "two"}
-- > toJSON x
-- Object (fromList [("url",String "two"),("title",String "one")])
-- > encode $ toJSON x
-- "{\"url\":\"two\",\"title\":\"one\"}"

instance ToJSON BookmarkJSON
    toJSON (BookmarkJSON title url) = object ["title", .= title, "url" .= url]

BookmarkJSONToBookmark :: BookmarkJSON -> Bookmark
BookmarkJSONToBookmark bookmarkJSON = Bookmark titleJSONToTitle urlJSONToUrl
    where
      titleJSONToTitle = fromMaybe "" $ bookmarkJSONUrl bookmarkJSON
      urlJSONToUrl = fromMaybe "" $ bookmarkJSONUrl bookmarkJSON

bookmarkAsJSONLBS :: Key Bookmark -> Bookmark -> Data.ByteString.Lazy.ByteString
bookmarkAsJSONLBS k b = encode . entityIdToJSON $ Entity k b