{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Servant.Common.Req
import Canonicalize

type MyAPI = "public" :> Get '[JSON] Text
        :<|> "private" :> BasicAuth "MyRealm" Text :> (SubAPI1 :<|> SubAPI2)
            
type SubAPI1 = "foo" :> (Get '[JSON] Text :<|> Post '[JSON] Text)
type SubAPI2 = "bar" :> (Get '[JSON] Text :<|> Post '[JSON] Text)

myAPI :: Proxy MyAPI
myAPI = Proxy

(getPublic
    :<|> getPrivateFoo
    :<|> postPrivateFoo 
    :<|> getPrivateBar
    :<|> postPrivateBar) = client (canonicalize myAPI)
