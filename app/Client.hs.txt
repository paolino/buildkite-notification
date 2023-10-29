{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, void)
import Control.Monad.Fix (fix)
import Data.Aeson (Value, (.:))
import Data.Bits (Bits (xor))
import Data.ByteString (ByteString, putStr)
import Data.JsonStream.Parser (ParseOutput (..), Parser, arrayOf, runParser, value)
import Network.Http.Client
    ( Hostname
    , Method (GET)
    , baselineContextSSL
    , buildRequest1
    , closeConnection
    , emptyBody
    , http
    , openConnection
    , openConnectionSSL
    , receiveResponse
    , sendRequest
    , setAccept
    , setHeader, put
    )
import qualified System.IO.Streams as Streams
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (putStr)
import qualified Data.ByteString.Lazy.Char8

host :: Hostname
host = "api.buildkite.com"

token :: ByteString
token = "bkua_de1c80d1f6294ca1d9737f8ffb14a9f0c1bd1ef2"

main :: IO ()
main = do
    b <- baselineContextSSL
    c <- openConnectionSSL b host 443

    let q = buildRequest1 $ do
            http GET "/v2/builds"
            setAccept "text/html"
            setHeader "Authorization" $ "Bearer " <> token

    sendRequest c q emptyBody

    void $ receiveResponse c $ \_p i -> ($ parser) $ fix $ \go r -> do
        case r of
            ParseYield v r' -> do
                Data.ByteString.Lazy.Char8.putStrLn $ encodePretty v
                Prelude.putStrLn "-------------------"
                go r'
            ParseNeedData k -> do
                xm <- Streams.read i
                case xm of
                    Nothing -> go $ k ""
                    Just x -> go $ k x
            ParseFailed e -> do
                print e
                return ()
            ParseDone _ -> do
                return ()

    closeConnection c

parser :: ParseOutput Value
parser = runParser $ arrayOf value