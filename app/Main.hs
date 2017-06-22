{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Base
import Control.Lens hiding (element)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.Environment (lookupEnv)
import Control.Exception (handle)

-- HTTP
import Network.HTTP.Simple
import Network.HTTP.Types.URI (urlEncode)

-- Slack
import Web.Slack
import Web.Slack.Message

-- XML
import Text.XML
import Text.XML.Cursor

-- Text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

-- Boardgamegeek Helpers
queryParam :: Text -> Text
queryParam = E.decodeUtf8 . urlEncode False . E.encodeUtf8

bggSearchUrl :: Text -> Text
bggSearchUrl = ("https://www.boardgamegeek.com/xmlapi2/search?type=boardgame&query=" <>) . queryParam

bggSearchUrlFromMention :: Text -> Text -> String
bggSearchUrlFromMention boardyId = T.unpack . bggSearchUrl . queryFromMention boardyId

bggBoardgameUrl :: Text -> Text
bggBoardgameUrl = ("https://www.boardgamegeek.com/boardgame/" <>)

bggApiItemIds :: Document -> [Text]
bggApiItemIds xml = fromDocument xml $/ element "item" >=> attribute "id"

bggSearchRequest :: Text -> Text -> IO (Maybe Text)
bggSearchRequest boardyId msg =
  handle ignoreHttpExceptions $ do
    req <- parseRequest (bggSearchUrlFromMention boardyId msg)
    resp <- httpLBS req
    return $! case parseLBS def (getResponseBody resp) of
                   Right xml | x:_ <- bggApiItemIds xml -> Just x
                   _ -> Nothing

ignoreHttpExceptions ::  HttpException -> IO (Maybe a)
ignoreHttpExceptions _ = return Nothing

-- Slack helpers
type ApiToken = String

boardyConfig :: ApiToken -> SlackConfig
boardyConfig apiToken = SlackConfig { _slackApiToken = apiToken }

userMentionFormat :: Text -> Text
userMentionFormat user_id = "<@" <> user_id <> ">"

isMessageToUser :: Text -> Text -> Bool
isMessageToUser = T.isPrefixOf . userMentionFormat

queryFromMention :: Text -> Text -> Text
queryFromMention = T.drop . T.length . userMentionFormat

selfId :: Getter (SlackState ()) Text
selfId = session . slackSelf . selfUserId . getId

messageGameId :: Text -> Text -> IO (Maybe Text)
messageGameId self msg = if isMessageToUser self msg
                            then bggSearchRequest self msg
                            else pure Nothing

sendGameMessage :: ChannelId -> Maybe Text -> Slack s ()
sendGameMessage cid (Just gameId) = sendMessage cid (bggBoardgameUrl gameId)
sendGameMessage _ _ = pure ()

boardy :: SlackBot ()
boardy (Message cid _ msg _ _ _) = do
  self <- use selfId
  gameId <- liftIO $ messageGameId self msg
  sendGameMessage cid gameId
boardy _ = pure ()

-- Main
main :: IO ()
main = do
  apiToken <- fromMaybe (error "SLACK_API_TOKEN not set") <$> lookupEnv "SLACK_API_TOKEN"
  runBot (boardyConfig apiToken) boardy ()
