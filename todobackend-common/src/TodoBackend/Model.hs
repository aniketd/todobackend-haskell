{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module TodoBackend.Model where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (fromMaybe)
import qualified Database.Persist.Class as DB
import qualified Database.Persist.Sqlite as Sqlite
import qualified Data.Text as Text
import Database.Persist.TH
import Web.PathPieces

type Latitude   = Double
type Longitude  = Double
-- data TodoLocation = TodoLocation Latitude Longitude deriving (Show)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
    title String
    completed Bool
    order Int
    latitude Latitude
    longitude Longitude
    deriving Show
|]

data TodoResponse = TodoResponse
  { trid        :: TodoId
  , trurl       :: String
  , trtitle     :: String
  , trcompleted :: Bool
  , trorder     :: Int
  , trlatitude  :: Latitude
  , trlongitude :: Longitude
  } deriving (Show)

$(deriveToJSON defaultOptions { fieldLabelModifier = drop 2}
  ''TodoResponse)

mkTodoResponse :: String -> Sqlite.Entity Todo -> TodoResponse
mkTodoResponse rootUrl (Sqlite.Entity key Todo{..}) =
    TodoResponse key todoUrl todoTitle todoCompleted todoOrder todoLatitude todoLongitude
  where
    todoUrl = rootUrl ++ "/todos/" ++ Text.unpack (toPathPiece key)

data TodoAction = TodoAction
  { actTitle :: Maybe String
  , actCompleted :: Maybe Bool
  , actOrder :: Maybe Int
  , actLatitude :: Maybe Latitude
  , actLongitude :: Maybe Longitude
  } deriving Show

instance FromJSON TodoAction where
  parseJSON (Object o) = TodoAction
    <$> o .:? "title"
    <*> o .:? "completed"
    <*> o .:? "order"
    <*> o .:? "latitude"
    <*> o .:? "longitude"
  parseJSON _ = mzero

instance ToJSON TodoAction where
  toJSON (TodoAction mTitle mCompl mOrder mLatitude mLongitude) = noNullsObject
      [ "title"     .= mTitle
      , "completed" .= mCompl
      , "order"     .= mOrder
      , "latitude"  .= mLatitude
      , "longitude" .= mLongitude
      ]
    where
      noNullsObject = object . filter notNull
      notNull (_, Null) = False
      notNull _         = True

actionToTodo :: TodoAction -> Todo
actionToTodo (TodoAction mTitle mCompleted mOrder mLatitude mLongitude) = Todo title completed order latitude longitude
  where
    title     = fromMaybe "" mTitle
    completed = fromMaybe False mCompleted
    order     = fromMaybe 0 mOrder
    latitude  = fromMaybe 0.0  mLatitude
    longitude = fromMaybe 0.0  mLongitude

actionToUpdates :: TodoAction -> [Sqlite.Update Todo]
actionToUpdates act =  updateTitle
                    ++ updateCompl
                    ++ updateOrd
                    ++ updateLatitude
                    ++ updateLongitude
  where
    updateTitle = maybe [] (\title -> [TodoTitle Sqlite.=. title])
                  (actTitle act)
    updateCompl = maybe [] (\compl -> [TodoCompleted Sqlite.=. compl])
                  (actCompleted act)
    updateOrd = maybe [] (\ord -> [TodoOrder Sqlite.=. ord])
                  (actOrder act)
    updateLatitude = maybe [] (\latitude -> [TodoLatitude Sqlite.=. latitude])
                  (actLatitude act)
    updateLongitude = maybe [] (\longitude -> [TodoLongitude Sqlite.=. longitude])
                  (actLongitude act)

runDb :: Sqlite.SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb = runNoLoggingT . runResourceT . Sqlite.withSqliteConn "dev.sqlite3" . Sqlite.runSqlConn
