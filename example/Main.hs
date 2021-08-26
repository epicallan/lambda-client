module Main where

import AWS.Lambda.Runtime
import Data.Aeson (FromJSON, parseJSON)
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Named = Named { name :: Text }
  deriving anyclass FromJSON
  deriving stock Generic

myHandler :: LambdaResponse -> IO (Either Text Text)
myHandler LambdaResponse{lambdaContext = LambdaContext{..}, ..} =
  case parseMaybe parseJSON event of
    Nothing -> return $ Left "My name is lambdaClient, what's yours?"
    Just (Named name) ->
      return . Right $ name <> " from " <> functionName static <> "!"

main :: IO ()
main = runtime myHandler
