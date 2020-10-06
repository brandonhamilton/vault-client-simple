{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Network.Vault
    ( AppRole(..)
    , AuthToken
    , AuthTokenInfo(..)
    , ErrorList(..)

    , vaultRequest

    , appRoleLogin
    , refreshToken
    , getKVWithToken

    , getKV
    )where

import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Aeson
import qualified Data.ByteString.Lazy      as LB
import           Data.HashMap.Strict       (HashMap)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import           GHC.Generics              (Generic)
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode, statusMessage)
import           Network.URI               (URI)

-- | Credentials for the AppRole Auth Method
data AppRole = AppRole
    { appRoleRoleId   :: !Text
    -- ^ The RoleID of the AppRole
    , appRoleSecretId :: !Text
    -- ^ The SecretID issued against the AppRole
    } deriving stock (Eq, Show, Generic)

instance ToJSON AppRole where
    toEncoding = genericToEncoding $ defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 7 }

instance FromJSON AppRole where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 7 }

-- | A token that can be used to authenticate requests
type AuthToken = Text

-- | Information about a token granted from Vault
data AuthTokenInfo = AuthTokenInfo
    { authTokenToken         :: AuthToken
    -- ^ The authentication token
    , authTokenRenewable     :: !Bool
    -- ^ Is this token renewable
    , authTokenLeaseDuration :: !Int
    -- ^ The lease duration of this token
    } deriving stock (Eq, Show, Generic)

instance FromJSON AuthTokenInfo where
    parseJSON = withObject "AuthTokenInfo" $ \o -> AuthTokenInfo
        <$> (o .: "auth" >>= (.: "client_token"))
        <*> (o .: "auth" >>= (.: "renewable"))
        <*> (o .: "auth" >>= (.: "lease_duration"))

-- | Data type for decoding JSON KV list
newtype KVList = KVList { unKVList :: HashMap Text Text }

instance FromJSON KVList where
    parseJSON = withObject "KVList" $ \o -> KVList <$> (o .: "data" >>= parseJSON)

-- | A list of errors
newtype ErrorList = ErrorList { errors :: [Text] }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Request a resource from Vault via the HTTP API
vaultRequest :: (MonadIO m, ToJSON a, FromJSON b) => Manager -> URI -> Maybe AuthToken -> Text -> Text -> Maybe a -> m (Either Text b)
vaultRequest manager uri token rMethod rPath body = do
    let request = (requestFromURI_ uri)
            { method = encodeUtf8 rMethod
            , path = encodeUtf8 rPath
            , requestHeaders = [("Content-Type", "application/json"), ("Accept", "application/json")] <> tokenHeader token
            , requestBody = RequestBodyLBS $ maybe LB.empty encode body
            }
    response <- liftIO $ httpLbs request manager
    case statusCode . responseStatus $ response of
        200 -> pure $ parseSuccess response
        _   -> pure $ Left $ parseVaultError response
    where
        tokenHeader (Just tok) = [("X-Vault-Token", encodeUtf8 tok)]
        tokenHeader Nothing    = []

        parseVaultError :: Response LB.ByteString -> Text
        parseVaultError res = let httpReason = decodeUtf8 . statusMessage . responseStatus $ res
                                  errorList = decode' .responseBody $ res :: Maybe ErrorList
                              in maybe httpReason (T.intercalate ", " . errors) errorList

        parseSuccess :: FromJSON b => Response LB.ByteString -> Either Text b
        parseSuccess res = let r = responseBody res in maybe (Left "Failure to parse response") Right $ decode' r

-- | Obtain a token using AppRole credentials
appRoleLogin :: MonadIO m => Manager -> URI -> AppRole -> m (Either Text AuthTokenInfo)
appRoleLogin manager uri appRole = vaultRequest manager uri Nothing "POST" "/v1/auth/approle/login" (Just appRole)

-- | Refresh an existing token
refreshToken :: MonadIO m => Manager -> URI -> AuthToken -> m (Either Text AuthToken)
refreshToken manager uri token = vaultRequest manager uri (Just token) "POST" "/v1/auth/token/renew-self" (Nothing :: Maybe ())

-- | Retrieve key/value secrets using an existing valid AuthToken
getKVWithToken :: MonadIO m => Manager -> URI -> AuthToken -> Text -> m (Either Text (HashMap Text Text))
getKVWithToken manager uri token kvPath = fmap unKVList <$> vaultRequest manager uri (Just token) "GET" ("/v1/kv/" <> kvPath) (Nothing :: Maybe ())

-- | Retrieve key/value secrets using an AppRole identity
getKV :: MonadIO m => Manager -> URI -> AppRole -> Text -> m (Either Text (HashMap Text Text))
getKV manager uri appRole kvPath = do
    tokenInfo <- appRoleLogin manager uri appRole
    case tokenInfo of
        Left e -> pure (Left e)
        Right (AuthTokenInfo token _ _) -> getKVWithToken manager uri token kvPath
