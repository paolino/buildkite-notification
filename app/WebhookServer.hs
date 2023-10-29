{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Configuration (Configuration, loadConfiguration)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Types ()
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.Time (getCurrentTime)
import Data.Yaml.Pretty (defConfig, encodePretty)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Notification (NotificationOrValue (..))
import Servant
    ( Capture
    , JSON
    , NoContent (NoContent)
    , Post
    , Proxy (..)
    , ReqBody
    , Server
    , serve
    , type (:>)
    )
import Slack (post)
import System.Environment (getEnv)
import System.IO
    ( BufferMode (..)
    , Handle
    , IOMode (..)
    , hSetBuffering
    , stdout
    , withFile
    )
import Prelude

type API =
    Capture "notification-id" String
        :> ReqBody '[JSON] NotificationOrValue
        :> Post '[JSON] NoContent

server :: Handle -> Configuration -> Server API
server handle cfg s (NotificationOrValue v mn) = do
    liftIO $ do
        t <- BL.pack . show <$> getCurrentTime
        BL.hPutStrLn handle $ "# Received on " <> t
        B.hPutStrLn handle $ encodePretty defConfig v
    case Map.lookup s cfg of
        Nothing -> liftIO $ putStrLn $ "No configuration found for " ++ s
        Just t -> do
            case mn of
                Just w -> do
                    -- pPrint w
                    -- pPrint $ renderNotification w
                    -- liftIO $ BL.putStrLn $ encodePretty $ renderBlocks $ renderNotification w
                    liftIO $ post t w
                    liftIO $ putStrLn "Posted"
                Nothing -> liftIO $ putStrLn "Failed to parse"
    return NoContent

userAPI :: Proxy API
userAPI = Proxy

main :: IO ()
main = do
    configFile <- getEnv "SERVICE_CONFIG_FILE"
    logInput <- getEnv "SERVICE_LOG_INPUT"
    hSetBuffering stdout LineBuffering
    configuration <- loadConfiguration configFile
    withFile logInput AppendMode $ \handle -> do
        hSetBuffering handle LineBuffering
        run 8081 $ logStdout $ serve userAPI $ server handle configuration
