module AWS.Lambda.Client
  ( HTTPConfig
  , LambdaClient
  , LambdaClientError (..)
  , LambdaResponse (..)
  , getHttpConfig
  , getNextLambdaResponse
  , sendEventError
  , sendEventSuccess
  , sendInitError
  )
where

import AWS.Lambda.Context
import AWS.Prelude
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT(..), liftEither, throwError, withExceptT)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Environment (lookupEnv)
import System.Envy (decodeEnv)

import qualified Data.Aeson          as JSON
import qualified Data.ByteString     as BS
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types  as HTTP

data HTTPConfig = HTTPConfig
  { request :: HTTP.Request
  , manager :: HTTP.Manager
  }

data LambdaResponse = LambdaResponse
  { event         :: JSON.Value
  , lambdaContext :: LambdaContext
  }
  deriving stock Show

data LambdaClientError
  = LambdaContextDecodeError Text
  | MissingLambdaRunTimeApi
  | InvalidLambdaRunTimeApi Text
  | InvalidStaticContext Text
  | ConnectionError HTTP.HttpException
  | ResponseBodyDecodeFailure Text
  | UnExpectedRunTimeError HTTP.Status Text
  | PayLoadTooLarge Text
  deriving stock Show

instance Exception LambdaClientError

type LambdaClient = ExceptT LambdaClientError IO

getHttpConfig :: LambdaClient HTTPConfig
getHttpConfig = do
  awsLambdaRuntimeApi <- ExceptT $ maybeToEither MissingLambdaRunTimeApi
    <$> lookupEnv "AWS_LAMBDA_RUNTIME_API"

  req <- withExceptT (InvalidLambdaRunTimeApi . showc @Text)
    . ExceptT
    . try @_ @HTTP.HttpException
    $ HTTP.parseRequest ("http://" <> awsLambdaRuntimeApi)

  man <- liftIO $ HTTP.newManager
    -- In the off chance that they set a proxy value, we don't want to
    -- use it.  There's also no reason to spend time reading env vars.
    $ HTTP.managerSetProxy HTTP.noProxy
    $ HTTP.defaultManagerSettings
    -- This is the most important setting, we must not timeout requests
    { HTTP.managerResponseTimeout = HTTP.responseTimeoutNone
    -- We only ever need a single connection, because we'll never make
    -- concurrent requests and never talk to more than one host.
    , HTTP.managerConnCount = 1
    , HTTP.managerIdleConnectionCount = 1
    }
  return $ HTTPConfig req man

getNextLambdaResponse :: HTTPConfig -> LambdaClient LambdaResponse
getNextLambdaResponse httpConfig = do
  nextRes <- getNextEvent httpConfig

  staticContext <- ExceptT $ pure . first (InvalidStaticContext . convert)
    =<< decodeEnv @StaticContext

  let mGetHeader headerName errorMsg = liftEither $ getHeader nextRes headerName errorMsg

  awsRequestId       <- RequestId <$> mGetHeader "Lambda-Runtime-Aws-Request-Id" "Missing request Id"
  xRayTraceId        <- mGetHeader "Lambda-Runtime-Trace-Id" "Missing trace Id"
  invokedFunctionArn <- mGetHeader "Lambda-Runtime-Invoked-Function-Arn" "Missing runtime Function"
  deadline           <- liftEither . toMillSeconds
    =<< mGetHeader "Lambda-Runtime-Deadline-Ms" "Missing lambda deadline"

  let clientContext = decodeOptional @ClientContext nextRes "Lambda-Runtime-Client-Context"
  let identity      = decodeOptional @CognitoIdentity nextRes "Lambda-Runtime-Cognito-Identity"

  let dynamicContext = DynamicContext {..}

  return LambdaResponse
    { event         = HTTP.responseBody nextRes
    , lambdaContext = LambdaContext staticContext dynamicContext
    }
  where
    toMillSeconds :: Text -> Either LambdaClientError UTCTime
    toMillSeconds ms = maybeToEither
      (LambdaContextDecodeError "Failed to decode lambdaRuntime milliseconds as utcTime") $ do
        milliseconds <- readMaybe @Double $ convert ms
        return $ posixSecondsToUTCTime $ realToFrac $ milliseconds / 1000

    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [(x,"")] -> Just x
      _        -> Nothing

    getHeader
      :: HTTP.Response JSON.Value
      -> HTTP.HeaderName
      -> Text
      -> Either LambdaClientError Text
    getHeader response header errorMsg
      = maybeToEither (LambdaContextDecodeError errorMsg)
      $ getResponseHeader header response

    decodeOptional
      :: forall a . JSON.FromJSON a
      => HTTP.Response JSON.Value
      -> HTTP.HeaderName
      -> Maybe a
    decodeOptional response = JSON.decodeStrict <=< flip getRawResponseHeader response

getNextEvent :: HTTPConfig -> LambdaClient (HTTP.Response JSON.Value)
getNextEvent HTTPConfig{..} = do
  response <- performHttpRequest $ HTTPConfig { request = toNextEventRequest request, .. }

  let statusCode = HTTP.responseStatus response

  unless (HTTP.statusIsSuccessful statusCode) . throwError
    $ UnExpectedRunTimeError statusCode "Could not retrieve next event"

  return response
  where
    toNextEventRequest :: HTTP.Request -> HTTP.Request
    toNextEventRequest req = req
      { HTTP.path = "2018-06-01/runtime/invocation/next"
      }

sendEventSuccess
  :: (MonadCatch m, MonadIO m)
  => HTTPConfig
  -> RequestId
  -> JSON.Value
  -> m ()
sendEventSuccess HTTPConfig{..} requestId json = do
  response <- catchConnectionError
    . flip HTTP.httpNoBody manager
    $ toEventSuccessRequest request

  let statusCode = HTTP.responseStatus response

  when (statusCode == HTTP.status413) . throwM . PayLoadTooLarge $ showc json

  unless (HTTP.statusIsSuccessful statusCode) .
    throwM $ UnExpectedRunTimeError statusCode "Could not post handler result"
  where
    toEventSuccessRequest :: HTTP.Request -> HTTP.Request
    toEventSuccessRequest req = req
      { HTTP.requestBody    = HTTP.RequestBodyLBS (JSON.encode json)
      , HTTP.method         = "POST"
      , HTTP.path           = "2018-06-01/runtime/invocation/" <> convert requestId <> "/response"
      }

sendEventError :: MonadIO m => HTTPConfig -> RequestId -> Text -> m ()
sendEventError HTTPConfig{..} requestId error
  = void
  . liftIO
  . flip HTTP.httpNoBody manager
  $ toEventErrorRequest request
  where
    toEventErrorRequest :: HTTP.Request -> HTTP.Request
    toEventErrorRequest req = (toBaseErrorRequest error req)
      { HTTP.path = "2018-06-01/runtime/invocation/" <> convert requestId <> "/error"
      }

sendInitError :: MonadIO m => HTTPConfig -> Text -> m ()
sendInitError HTTPConfig{..} error
  = void
  . liftIO
  . flip HTTP.httpNoBody manager
  $ toInitErrorRequest request
  where
    toInitErrorRequest :: HTTP.Request -> HTTP.Request
    toInitErrorRequest req = (toBaseErrorRequest error req)
      { HTTP.path = "2018-06-01/runtime/init/error"
      }

performHttpRequest :: HTTPConfig -> LambdaClient (HTTP.Response JSON.Value)
performHttpRequest HTTPConfig{..} = do
  response <- catchConnectionError $ HTTP.httpLbs request manager

  body <- liftEither
    . first (ResponseBodyDecodeFailure . convert)
    . JSON.eitherDecode
    $ HTTP.responseBody response

  pure $ fmap (const body) response

catchConnectionError :: (MonadCatch m, MonadIO m) => IO a -> m a
catchConnectionError action =
  catch (liftIO action)
    $ \e -> throwM . ConnectionError $ (e :: HTTP.HttpException)

toBaseErrorRequest :: Text -> HTTP.Request -> HTTP.Request
toBaseErrorRequest error req = req
  { HTTP.requestBody    = HTTP.RequestBodyLBS (JSON.encode error)
  , HTTP.method         = "POST"
  , HTTP.requestHeaders =  [("Content-Type", "application/vnd.aws.lambda.error+json") ]
  }

getResponseHeader :: HTTP.HeaderName -> HTTP.Response a -> Maybe Text
getResponseHeader name = fmap decodeUtf8 . getRawResponseHeader name

getRawResponseHeader :: HTTP.HeaderName -> HTTP.Response a -> Maybe BS.ByteString
getRawResponseHeader name
  = safeHead
  . map snd
  . filter ((name ==) . fst)
  . HTTP.responseHeaders
