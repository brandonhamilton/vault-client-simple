{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Aeson
import           Data.HashMap.Strict
import           Network.HTTP.Mock
import           Network.HTTP.Types.Status
import           Network.URI
import           Network.Vault
import           Network.Wai
import           Test.Hspec

mockURI :: URI
mockURI = URI "http:" (Just (URIAuth {uriUserInfo = "", uriRegName = "localhost", uriPort = ""})) "/" "" ""

-- | A mock server
mockServer :: Application
mockServer req respond =
    case rawPathInfo req of
        "/v1/auth/approle/login" -> do
            body <-lazyRequestBody req
            case decode body of
                Nothing -> badReq "missing role_id"
                Just (AppRole "" "") -> badReq "missing role_id"
                Just (AppRole "<WRONG>" "<WRONG>") -> badReq "invalid role ID"
                Just (AppRole "<RIGHT>" "<WRONG>") -> badReq "invalid secret id"
                Just (AppRole "<RIGHT>" "<RIGHT>") -> goodReq "{\"request_id\":\"e8851eff-c509-3d81-a14c-436c68257595\",\"lease_id\":\"\",\"renewable\":false,\"lease_duration\":0,\"data\":null,\"wrap_info\":null,\"warnings\":null,\"auth\":{\"client_token\":\"s.z7w4siStV3XkTabES4ghcqwo\",\"accessor\":\"FAlhmT829oXq8BmjEplfYSXf\",\"policies\":[\"default\",\"example\"],\"token_policies\":[\"default\",\"example\"],\"metadata\":{\"role_name\":\"example-role\"},\"lease_duration\":120,\"renewable\":true,\"entity_id\":\"7120e4ea-1a8b-1d2a-3761-bc149a70e334\",\"token_type\":\"service\",\"orphan\":true}}\n"
                _ -> badReq "invalid request"
        "/v1/kv/test/secrets" -> do
            case Prelude.lookup "X-Vault-Token" (requestHeaders req) of
                Just "s.z7w4siStV3XkTabES4ghcqwo" -> goodReq "{\"request_id\":\"eb759e7a-f507-9caf-f02f-68d208d9a3ba\",\"lease_id\":\"\",\"renewable\":false,\"lease_duration\":2764800,\"data\":{\"secretKey\":\"<SECRET>\"},\"wrap_info\":null,\"warnings\":null,\"auth\":null}\n"
                _ -> badReq "invalid request"
        _ -> respond $ responseLBS status404 [] ""
    where
        badReq err = respond $ responseLBS status400 [("Content-Type", "application/json")] (encode (ErrorList [err]))
        goodReq res = respond $ responseLBS status200 [("Content-Type", "application/json")] res

main :: IO ()
main = withMockedManager mockServer $ \manager ->
    hspec $ do
        describe "AppRole access" $ do
            it "handles error responses" $ do
                appRoleLogin manager mockURI (AppRole "" "") `shouldReturn` Left "missing role_id"
                appRoleLogin manager mockURI (AppRole "<WRONG>" "<WRONG>") `shouldReturn` Left "invalid role ID"
                appRoleLogin manager mockURI (AppRole "<RIGHT>" "<WRONG>") `shouldReturn` Left "invalid secret id"
            it "succesfully retrieves a token" $ do
                appRoleLogin manager mockURI (AppRole "<RIGHT>" "<RIGHT>") `shouldReturn` Right (AuthTokenInfo {authTokenToken = "s.z7w4siStV3XkTabES4ghcqwo", authTokenRenewable = True, authTokenLeaseDuration = 120})
        describe "Fetch key/value secrets" $ do
            it "succesfully retrievea a map of key/values" $ do
                getKV  manager mockURI (AppRole "<RIGHT>" "<RIGHT>") "test/secrets" `shouldReturn` Right (fromList [("secretKey","<SECRET>")])
