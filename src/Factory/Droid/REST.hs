{-# LANGUAGE OverloadedStrings #-}

-- | Native Factory REST helpers. Each response is scoped; the reusable HTTP
-- manager owns its connection pool and releases idle connections automatically.
module Factory.Droid.REST
  ( RestClient,
    RestOptions (..),
    defaultRestOptions,
    RestError (..),
    newRestClient,
    PageOptions (..),
    defaultPageOptions,
    listMachineTemplates,
    getMachineTemplate,
    listComputers,
    getComputer,
    getComputerByName,
    createComputer,
    updateComputer,
    deleteComputer,
    restartComputer,
    refreshComputer,
    getComputerMetrics,
    retryInstallDeps,
    listRemoteSessions,
  )
where

import Control.Exception (Exception, throwIO, try)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (..), eitherDecodeStrict', encode, withObject, (.:))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Factory.Droid.Schema.REST
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Method, Query, hAccept, hAuthorization, hContentType, statusCode)
import Network.HTTP.Types.URI (renderQuery, urlEncode)
import Network.URI (URI (..), URIAuth (..), parseURI)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data RestOptions = RestOptions
  { restApiKey :: !Text,
    restBaseUrl :: !Text,
    restTimeoutMicros :: !(Maybe Int),
    restResponseLimitBytes :: !Int
  }

instance Show RestOptions where show _ = "RestOptions <redacted>"

defaultRestOptions :: Text -> RestOptions
defaultRestOptions key = RestOptions key "https://api.factory.ai" (Just 30000000) (10 * 1024 * 1024)

data RestError
  = RestInvalidOptions
  | RestInvalidIdentifier
  | RestTransportFailure
  | RestTimedOut
  | RestResponseTooLarge
  | RestNonJsonResponse !Int
  | RestUnexpectedResponse !Int
  | RestAuthenticationError !Int !(Maybe Text)
  | RestApiError !Int !(Maybe Text)
  deriving stock (Eq)

instance Show RestError where
  show = \case
    RestInvalidOptions -> "RestInvalidOptions"
    RestInvalidIdentifier -> "RestInvalidIdentifier"
    RestTransportFailure -> "RestTransportFailure"
    RestTimedOut -> "RestTimedOut"
    RestResponseTooLarge -> "RestResponseTooLarge"
    RestNonJsonResponse code -> "RestNonJsonResponse " <> show code
    RestUnexpectedResponse code -> "RestUnexpectedResponse " <> show code
    RestAuthenticationError code _ -> "RestAuthenticationError " <> show code <> " <redacted>"
    RestApiError code _ -> "RestApiError " <> show code <> " <redacted>"

instance Exception RestError

data RestClient = RestClient !HTTP.Manager !HTTP.Request !ByteString !(Maybe Int) !Int

instance Show RestClient where show _ = "RestClient <redacted>"

-- | Explicit credentials only. HTTPS verifies certificates; an HTTP base URL
-- is an explicit plaintext choice. User info, queries and fragments are rejected.
-- Redirects and automatic transport retries are disabled: a response cannot move
-- credentials to another endpoint or repeat a possibly completed mutation.
newRestClient :: RestOptions -> IO RestClient
newRestClient options = do
  let invalid :: IO a
      invalid = throwIO RestInvalidOptions
  unless (not (Text.null (restApiKey options)) && Text.all (\c -> c >= '!' && c <= '~') (restApiKey options)) invalid
  when (restResponseLimitBytes options <= 0 || maybe False (< 0) (restTimeoutMicros options)) invalid
  uri <- maybe invalid pure (parseURI (Text.unpack (restBaseUrl options)))
  auth <- maybe invalid pure (uriAuthority uri)
  unless (null (uriPort auth) || maybe False (\port -> port > 0 && port <= 65535) (readMaybe (drop 1 (uriPort auth)) :: Maybe Integer)) invalid
  unless (map toLower (uriScheme uri) `elem` ["https:", "http:"] && null (uriUserInfo auth) && not (null (uriRegName auth)) && null (uriQuery uri) && null (uriFragment uri)) invalid
  parsed <- try @HTTP.HttpException (HTTP.parseRequest (Text.unpack (restBaseUrl options)))
  base <- either (const invalid) pure parsed
  manager <- HTTP.newManager (HTTP.managerSetProxy HTTP.noProxy (tlsManagerSettings {HTTP.managerRetryableException = const False}))
  let configured = base {HTTP.requestHeaders = [(hAuthorization, "Bearer " <> Text.encodeUtf8 (restApiKey options)), (hAccept, "application/json")], HTTP.redirectCount = 0, HTTP.checkResponse = \_ _ -> pure (), HTTP.responseTimeout = HTTP.responseTimeoutNone, HTTP.cookieJar = Nothing}
  pure (RestClient manager configured (Text.encodeUtf8 (Text.pack (uriPath uri))) (restTimeoutMicros options) (restResponseLimitBytes options))

data PageOptions = PageOptions
  { pageLimit :: !(Maybe Scientific),
    pageCursor :: !(Maybe Text)
  }
  deriving stock (Eq)

instance Show PageOptions where show _ = "PageOptions <redacted>"

defaultPageOptions :: PageOptions
defaultPageOptions = PageOptions Nothing Nothing

listMachineTemplates :: RestClient -> PageOptions -> IO (Page MachineTemplate)
listMachineTemplates client options = request client "GET" ["machines", "templates"] (pageQuery options) Nothing (pageParser "templates")

getMachineTemplate :: RestClient -> Text -> IO MachineTemplate
getMachineTemplate client ident = resource client "GET" ["machines", "templates"] ident [] Nothing parseJSON

listComputers :: RestClient -> IO [Computer]
listComputers client = request client "GET" ["computers"] [] Nothing (withObject "Computers" (.: "computers"))

getComputer :: RestClient -> Text -> IO Computer
getComputer client ident = resource client "GET" ["computers"] ident [] Nothing parseJSON

getComputerByName :: RestClient -> Text -> IO Computer
getComputerByName client name = resource client "GET" ["computers", "name"] name [] Nothing parseJSON

createComputer :: RestClient -> CreateComputerParams -> IO Computer
createComputer client params = request client "POST" ["computers"] [] (Just (toJSON params)) parseJSON

updateComputer :: RestClient -> Text -> UpdateComputerParams -> IO Computer
updateComputer client ident params = resource client "PATCH" ["computers"] ident [] (Just (toJSON params)) parseJSON

deleteComputer :: RestClient -> Text -> IO ()
deleteComputer client ident = do
  checkIdentifier ident
  voidRequest client "DELETE" ["computers", ident]

restartComputer :: RestClient -> Text -> IO ()
restartComputer client ident = do
  checkIdentifier ident
  voidRequest client "POST" ["computers", ident, "restart"]

refreshComputer :: RestClient -> Text -> IO Integer
refreshComputer client ident = do
  checkIdentifier ident
  request client "POST" ["computers", ident, "refresh"] [] Nothing (withObject "Refreshed computers" (.: "configured"))

getComputerMetrics :: RestClient -> Text -> Maybe Text -> IO [ComputerMetric]
getComputerMetrics client ident start = do
  checkIdentifier ident
  request client "GET" ["computers", ident, "metrics"] (textQuery "start" start) Nothing parseJSON

retryInstallDeps :: RestClient -> Text -> IO Computer
retryInstallDeps client ident = do
  checkIdentifier ident
  request client "POST" ["computers", ident, "install-deps"] [] Nothing parseJSON

listRemoteSessions :: RestClient -> Maybe Text -> PageOptions -> IO (Page RemoteSession)
listRemoteSessions client computer options = request client "GET" ["sessions"] (textQuery "computerId" computer <> pageQuery options) Nothing (pageParser "sessions")

resource :: RestClient -> Method -> [Text] -> Text -> Query -> Maybe Value -> (Value -> Parser a) -> IO a
resource client method prefix ident query body parser = do
  checkIdentifier ident
  request client method (prefix <> [ident]) query body parser

checkIdentifier :: Text -> IO ()
checkIdentifier ident = when (Text.null ident || ident `elem` [".", ".."]) (throwIO RestInvalidIdentifier)

pageQuery :: PageOptions -> Query
pageQuery options = maybe [] (\n -> [("limit", Just (BL.toStrict (encode n)))]) (pageLimit options) <> textQuery "cursor" (pageCursor options)

textQuery :: ByteString -> Maybe Text -> Query
textQuery key = maybe [] (\value -> [(key, Just (Text.encodeUtf8 value))])

pageParser :: (FromJSON a) => Key -> Value -> Parser (Page a)
pageParser key = withObject "Page" $ \v -> Page <$> v .: key <*> v .: "pagination"

request :: RestClient -> Method -> [Text] -> Query -> Maybe Value -> (Value -> Parser a) -> IO a
request client method path query body parser = perform client method path query body $ \code -> \case
  Nothing -> throwIO (RestUnexpectedResponse code)
  Just value -> either (const (throwIO (RestUnexpectedResponse code))) pure (parseEither parser value)

voidRequest :: RestClient -> Method -> [Text] -> IO ()
voidRequest client method path = perform client method path [] Nothing (\_ _ -> pure ())

perform :: RestClient -> Method -> [Text] -> Query -> Maybe Value -> (Int -> Maybe Value -> IO a) -> IO a
perform (RestClient manager base prefix deadline limit) method path query body consume = do
  let outgoing = base {HTTP.method = method, HTTP.path = prefix <> "/api/v0/" <> BS.intercalate "/" (map (urlEncode True . Text.encodeUtf8) path), HTTP.queryString = renderQuery True query, HTTP.requestBody = maybe (HTTP.RequestBodyBS BS.empty) (HTTP.RequestBodyLBS . encode) body, HTTP.requestHeaders = maybe [] (const [(hContentType, "application/json")]) body <> HTTP.requestHeaders base}
      exchange = HTTP.withResponse outgoing manager $ \response -> do
        let code = statusCode (HTTP.responseStatus response)
        if code == 204
          then consume code Nothing
          else do
            bytes <- readBounded limit (HTTP.responseBody response)
            let jsonBytes = fromMaybe bytes (BS.stripPrefix "\xef\xbb\xbf" bytes)
            value <- either (const (throwIO (RestNonJsonResponse code))) pure (eitherDecodeStrict' jsonBytes)
            if code >= 200 && code < 300
              then consume code (Just value)
              else do
                let message = case value of Object fields | Just (String text) <- KeyMap.lookup "error" fields -> Just text; _ -> Nothing
                throwIO (if code == 401 || code == 403 then RestAuthenticationError code message else RestApiError code message)
      timed = maybe exchange (\micros -> timeout micros exchange >>= maybe (throwIO RestTimedOut) pure) deadline
  outcome <- try @HTTP.HttpException (HTTP.managerWrapException tlsManagerSettings outgoing timed)
  either (const (throwIO RestTransportFailure)) pure outcome

readBounded :: Int -> HTTP.BodyReader -> IO ByteString
readBounded limit reader = go limit []
  where
    go remaining chunks = do
      chunk <- HTTP.brRead reader
      if BS.null chunk
        then pure (BS.concat (reverse chunks))
        else do
          when (BS.length chunk > remaining) (throwIO RestResponseTooLarge)
          go (remaining - BS.length chunk) (chunk : chunks)
