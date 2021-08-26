{-# LANGUAGE DerivingVia #-}
module AWS.Lambda.Context where

import AWS.Prelude
import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word16)

import qualified Data.ByteString        as BS
import qualified Data.Time              as Time
import qualified Data.Time.Clock.POSIX  as Time
import qualified System.Envy            as Envy

data ClientApplication = ClientApplication
  { appTitle       :: Text
  , appVersionName :: Text
  , appVersionCode :: Text
  , appPackageName :: Text
  }
  deriving anyclass (ToJSON, FromJSON)
  deriving stock (Show, Generic)

data ClientContext = ClientContext
  { client      :: ClientApplication
  , custom      :: Map Text Text
  , environment :: Map Text Text
  }
  deriving anyclass (ToJSON, FromJSON)
  deriving stock (Show, Generic)

data CognitoIdentity = CognitoIdentity
  { identityId     :: Text
  , identityPoolId :: Text
  }
  deriving anyclass (ToJSON, FromJSON)
  deriving stock (Show, Generic)

data StaticContext = StaticContext
  { functionName       :: Text
  , functionVersion    :: Text
  , functionMemorySize :: Word16
  , logGroupName       :: Text
  , logStreamName      :: Text
  }
  deriving anyclass (ToJSON, FromJSON)
  deriving stock (Show, Generic)

instance Envy.DefConfig StaticContext where
  defConfig = StaticContext mempty mempty 0 mempty mempty

instance Envy.FromEnv StaticContext where
  fromEnv = Envy.gFromEnvCustom Envy.Option
    { dropPrefixCount = 0
    , customPrefix = "AWS_LAMBDA"
    }

newtype RequestId = RequestId Text
  deriving (Semigroup, Monoid, ToJSON, FromJSON) via Text
  deriving stock Show

instance Conversion BS.ByteString RequestId where
  convert (RequestId requestId) = encodeUtf8 requestId

data DynamicContext = DynamicContext
  { awsRequestId       :: RequestId
  , invokedFunctionArn :: Text
  , xRayTraceId        :: Text
  , deadline           :: Time.UTCTime
  , clientContext      :: Maybe ClientContext
  , identity           :: Maybe CognitoIdentity
  }
  deriving anyclass (ToJSON, FromJSON)
  deriving stock (Show, Generic)


instance Envy.DefConfig DynamicContext where
  defConfig = DynamicContext mempty mempty mempty (Time.posixSecondsToUTCTime 0) empty empty

data LambdaContext = LambdaContext
  { static  :: StaticContext
  , dynamic :: DynamicContext
  }
  deriving anyclass (ToJSON, FromJSON)
  deriving stock (Show, Generic)

instance Envy.DefConfig LambdaContext where
  defConfig = LambdaContext Envy.defConfig Envy.defConfig
