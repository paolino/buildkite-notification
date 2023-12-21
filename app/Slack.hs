{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Slack where

import Control.Monad.Catch (MonadCatch, SomeException (..), catch)
import Control.Monad.Free (Free)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (KeyValue (..), Value (..), object)
import Data.Aeson.Types (Pair)
import Data.ByteString.Char8 as B (putStrLn)
import Data.String (IsString)
import Data.Text as T (Text, pack)
import Network.HTTP.Req
    ( POST (POST)
    , ReqBodyJson (ReqBodyJson)
    , bsResponse
    , defaultHttpConfig
    , req
    , responseBody
    , runReq
    , useHttpsURI
    )
import Notification
    ( Build (..)
    , Event (..)
    , Notification (..)
    , Pipeline (..)
    , Provider (..)
    , Settings (..)
    , State (..)
    )
import Text.URI (mkURI)

isInteresting :: Notification -> Bool
isInteresting Notification{event = matchEvent -> t} = t

matchEvent :: Event -> Bool
matchEvent BuildFailing = True
matchEvent BuildFinished = True
matchEvent _ = False

postGuarded :: (MonadCatch m, MonadIO m) => m a -> m (Either Text a)
postGuarded post' = catch
    (Right <$> post')
    $ \e@(SomeException _) -> do
        liftIO $ print e
        pure $ Left $ T.pack $ show e

post :: String -> Notification -> IO ()
post whe wha@(isInteresting -> True) = runReq defaultHttpConfig $ do
    let murl = mkURI (T.pack whe) >>= useHttpsURI
        whaf = renderNotification wha
    case murl of
        Nothing -> liftIO $ Prelude.putStrLn "Invalid URL"
        Just url -> do
            let post' x =
                    req
                        POST
                        (fst url)
                        (ReqBodyJson $ renderBlocks x)
                        bsResponse
                        mempty
            mbs <- postGuarded $ post' whaf
            case mbs of
                Left e -> do
                    _ <- postGuarded $ post' $ oneBlock e
                    liftIO $ Prelude.putStrLn "Failed to post"
                Right bs -> liftIO $ B.putStrLn (responseBody bs)
post _ _ = pure ()

oneBlock :: Text -> Blocks
oneBlock t = [SectionBlock $ Section (Just $ MarkdownText t) []]

type Blocks = [Block]

data Block
    = SectionBlock Section
    | DividerBlock
    | ContextBlock Context
    | RichTextBlock RichText
    deriving (Show)

data Section = Section
    { textSection :: Maybe Content
    , fields :: [Content]
    }
    deriving (Show)

data Content = PlainText Text | MarkdownText Text
    deriving (Show)

newtype Context = Context
    { elementsContext :: [Content]
    }
    deriving (Show)

newtype RichText = RichText
    { elementsRichText :: [RichTextSection]
    }
    deriving (Show)

newtype RichTextSection = RichTextSection
    { elementsRichTextSection :: [RichTextElement]
    }
    deriving (Show)

data RichTextElement = RichTextElement
    { textRichTextElement :: Text
    , style :: [Style]
    }
    deriving (Show)

data Style = Bold | Italic | Strike
    deriving (Show)

renderBlocks :: Blocks -> Value
renderBlocks bs = object ["blocks" .= fmap renderBlock bs]

renderBlock :: Block -> Value
renderBlock DividerBlock = object ["type" .= ("divider" :: Text)]
renderBlock (SectionBlock s) =
    object
        [ "type" .= ("section" :: Text)
        , "text" .= (renderContent <$> textSection s)
        , "fields" .= fmap renderContent (fields s)
        ]
renderBlock (ContextBlock c) =
    object
        [ "type" .= ("context" :: Text)
        , "elements" .= fmap renderContent (elementsContext c)
        ]
renderBlock (RichTextBlock r) =
    object
        [ "type" .= ("rich_text" :: Text)
        , "elements" .= fmap renderRichTextSection (elementsRichText r)
        ]

renderRichTextSection :: RichTextSection -> Value
renderRichTextSection (RichTextSection rs) =
    object
        [ "type" .= ("rich_text_section" :: Text)
        , "elements" .= fmap renderRichTextElement rs
        ]

renderRichTextElement :: RichTextElement -> Value
renderRichTextElement (RichTextElement t s) =
    object
        [ "type" .= ("text" :: Text)
        , "text" .= t
        , "style" .= renderStyle s
        ]

renderStyle :: [Style] -> Value
renderStyle xs = object $ fmap renderStyle1 xs

renderStyle1 :: Style -> Pair
renderStyle1 Bold = "bold" .= True
renderStyle1 Italic = "italic" .= True
renderStyle1 Strike = "strike" .= True

renderContent :: Content -> Value
renderContent (PlainText t) =
    object
        [ "type" .= ("plain_text" :: Text)
        , "text" .= t
        ]
renderContent (MarkdownText t) =
    object
        [ "type" .= ("mrkdwn" :: Text)
        , "text" .= t
        ]

--------------------------------------------------------

bold :: (Semigroup a, IsString a) => a -> a
bold x = "*" <> x <> "*"

italic :: (Semigroup a, IsString a) => a -> a
italic x = "_" <> x <> "_"

link :: (Semigroup a, IsString a) => a -> a -> a
link x y = "<" <> x <> "|" <> y <> ">"

renderNotification :: Notification -> Blocks
renderNotification
    Notification
        { pipeline =
            Pipeline
                { description
                , repository = _repository_link
                , provider =
                    Provider
                        { id = _id'
                        , settings = Settings{repository = _repository}
                        }
                }
        , build = Build{state = state, commit, branch, web_url}
        , event
        } =
        [ RichTextBlock
            $ RichText
                [ RichTextSection
                    [ RichTextElement
                        { textRichTextElement =
                            result state
                                <> " "
                                <> T.pack (missing branch)
                        , style = []
                        }
                    ]
                ]
        , SectionBlock
            $ Section
                { textSection =
                    Just
                        $ MarkdownText
                        $ bold (renderEvent event)
                            <> "\n"
                            <> italic (T.pack description)
                , fields =
                    [ MarkdownText $ link (T.pack web_url) "Build"
                    , MarkdownText $ T.pack $ missing commit
                    ]
                }
        ]

renderEvent :: Event -> Text
renderEvent BuildFailing = "Build is failing"
renderEvent BuildFinished = "Build has finished"
renderEvent BuildScheduled = "Build has been scheduled"
renderEvent BuildRunning = "Build is running"
renderEvent JobScheduled = "Job has been scheduled"
renderEvent JobRunning = "Job is running"
renderEvent JobFailing = "Job is failing"
renderEvent JobFinished = "Job has finished"
renderEvent (UnknownEvent x) = "Unknown event " <> x

result :: State -> Text
result Failed = ":large_red_square:"
result Failing = ":large_red_square:"
result Passed = ":large_green_square:"
result Running = ":large_blue_square:"
result Canceled = ":large_yellow_square:"
result Blocked = ":large_yellow_square:"
result (Unknown x) = ":question:" <> x

-- <> [ SectionBlock
--     $ Section
--         { textSection =
--             Just
--                 $ MarkdownText
--                 $ link (T.pack l) "Repository"
--         , fields = []
--         }
--    | l <- maybeToList repository_link
--    ]

missing :: (IsString a) => Maybe a -> a
missing Nothing = "-----"
missing (Just x) = x

data Appenda a r where
    Empty :: Appenda a a
    Cons :: a -> r -> Appenda a r

type AppendF a = Free (Appenda a)

{-
runAppenda = foldFree f
    where
        f Empty = []
        f (Cons a r) = a : r -}
