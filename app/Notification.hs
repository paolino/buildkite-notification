{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Notification where

import Control.Applicative (optional)
import Data.Aeson (FromJSON (parseJSON), ToJSON (..), Value, withText)
import Data.Text (Text)
import GHC.Generics (Generic)

data Build = Build
    { state :: State
    , created_at :: String
    , scheduled_at :: Maybe String
    , started_at :: Maybe String
    , finished_at :: Maybe String
    , web_url :: String
    , commit :: Maybe String
    , branch :: Maybe String
    , creator :: Maybe Creator
    , message :: String
    , number :: Int
    }
    deriving (Show, Generic)

instance FromJSON Build


data Creator = Creator
    { id :: String
    , name :: String
    , email :: String
    }
    deriving (Show, Generic)

instance FromJSON Creator

data State
    = Running
    | Failed
    | Failing
    | Passed
    | Canceled
    | Blocked
    | Unknown Text
    deriving (Show, Generic)

isNotCanceled :: State -> Bool
isNotCanceled Canceled = False
isNotCanceled _ = True

instance FromJSON State where
    parseJSON = withText "State" $ \case
        "running" -> pure Running
        "failed" -> pure Failed
        "failing" -> pure Failing
        "passed" -> pure Passed
        "canceled" -> pure Canceled
        "blocked" -> pure Blocked
        x -> pure $ Unknown x

data Pipeline = Pipeline
    { created_at :: String
    , description :: String
    , id :: Maybe String
    , repository :: Maybe String
    , provider :: Provider
    }
    deriving (Show, Generic)

instance FromJSON Pipeline

data Notification = Notification
    { event :: Event
    , pipeline :: Pipeline
    , build :: Build
    }
    deriving (Show, Generic)


instance FromJSON Notification

data Event
    = BuildScheduled
    | BuildRunning
    | BuildFailing
    | BuildFinished
    | JobScheduled
    | JobRunning
    | JobFailing
    | JobFinished
    | UnknownEvent Text
    deriving (Show, Generic)

instance FromJSON Event where
    parseJSON = withText "Event" $ \t -> case t of
        "build.scheduled" -> pure BuildScheduled
        "build.running" -> pure BuildRunning
        "build.failing" -> pure BuildFailing
        "build.finished" -> pure BuildFinished
        "job.scheduled" -> pure JobScheduled
        "job.running" -> pure JobRunning
        "job.failing" -> pure JobFailing
        "job.finished" -> pure JobFinished
        _ -> pure $ UnknownEvent t

data Provider = Provider
    { id :: String
    , settings :: Settings
    }
    deriving (Show, Generic)

instance FromJSON Provider

instance ToJSON Provider

newtype Settings = Settings
    { repository :: String
    }
    deriving (Show, Generic)

instance FromJSON Settings

instance ToJSON Settings

data NotificationOrValue = NotificationOrValue
    { value :: Value
    , notification :: Maybe Notification
    }
    deriving (Show, Generic)

instance FromJSON NotificationOrValue where
    parseJSON v = NotificationOrValue v <$> optional (parseJSON v)