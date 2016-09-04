{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Canonicalize

type MyAPI = "public" :> Get '[JSON] Text
        :<|> "private" :> BasicAuth "MyRealm" Text :> ("all" :> Get '[JSON] Text
                                                  :<|> "foo" :> Capture "bar" Text :> (Get '[JSON] Text 
                                                                                  :<|> Post '[JSON] Text))

myAPI :: Proxy MyAPI
myAPI = Proxy

getPublicText :: Manager -> BaseUrl -> ClientM Text
getPrivateAllText :: BasicAuthData -> Manager -> BaseUrl -> ClientM Text
getPrivateFooBarText :: BasicAuthData -> Text -> Manager -> BaseUrl -> ClientM Text
postPrivateFooBarText :: BasicAuthData -> Text -> Manager -> BaseUrl -> ClientM Text

(getPublicText 
    :<|> getPrivateAllText 
    :<|> getPrivateFooBarText 
    :<|> postPrivateFooBarText) = client (canonicalize myAPI)

