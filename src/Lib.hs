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
        :<|> "private" :> BasicAuth "testParam" Text :> (Get '[JSON] Text 
                                                  :<|> Post '[JSON] Text)

myAPI :: Proxy MyAPI
myAPI = Proxy


getPublicText :: Manager -> BaseUrl -> ClientM Text
getPrivateText :: BasicAuthData -> Manager -> BaseUrl -> ClientM Text
postPrivateText :: BasicAuthData -> Manager -> BaseUrl -> ClientM Text

canonicalAPI :: Proxy (Canonicalize MyAPI)
canonicalAPI = canonicalize myAPI

(getPublicText :<|> getPrivateText :<|> postPrivateText) = client (canonicalize myAPI)

