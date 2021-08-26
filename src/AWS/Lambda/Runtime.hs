module AWS.Lambda.Runtime
  ( module Exports
  , LambdaClientError (..)
  , LambdaResponse (..)
  , runtime
  )
where

import AWS.Lambda.Client
import AWS.Lambda.Context as Exports
import AWS.Prelude
import Control.Monad.Except (runExceptT)
import Data.Aeson (ToJSON(..))

type LambdaFunction m result = LambdaResponse -> m (Either Text result)

runtime
  :: forall m result . (MonadCatch m, MonadIO m, ToJSON result)
  => LambdaFunction m result
  -> m ()
runtime = forever . runtimeLoop

runtimeLoop
  :: forall result m . (ToJSON result, MonadIO m, MonadCatch m)
  => LambdaFunction m result
  -> m ()
runtimeLoop fn =
  either throwM runFunction =<< liftIO (runExceptT getHttpConfig)
  where
    runFunction :: HTTPConfig -> m ()
    runFunction httpConfig = do
      lambdaResponse <- processNextLambdaAction httpConfig
        $ getNextLambdaResponse httpConfig
      result         <- fn lambdaResponse

      let requestId = getRequestId lambdaResponse

      either
        (sendEventError httpConfig requestId)
        (sendEventSuccess httpConfig requestId . toJSON) result

    processNextLambdaAction :: HTTPConfig -> LambdaClient a -> m a
    processNextLambdaAction httpConfig action =
      liftIO (runExceptT action) >>= \case
        Right result -> pure result
        Left error   -> do
          sendInitError httpConfig $ showc error
          throwM error

    getRequestId :: LambdaResponse -> RequestId
    getRequestId = awsRequestId . dynamic . lambdaContext
