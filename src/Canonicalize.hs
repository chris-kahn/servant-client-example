{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module Canonicalize where

import           Control.Applicative
import           Data.Proxy               (Proxy(..))
import           GHC.TypeLits
import           Servant.API.Alternative  ((:<|>) (..))
import           Servant.API.Sub          ((:>))

-- | Turn an API type into its canonical form.
--
-- The canonical form of an API type is basically the all-flattened form
-- of the original type. More formally, it takes a type as input and hands you
-- back an /equivalent/ type formed of toplevel `:<|>`-separated chains of `:>`s,
-- i.e with all `:>`s distributed inside the `:<|>`s.
--
-- It basically turns:
--
-- > "hello" :> (Get Hello :<|> ReqBody Hello :> Put Hello)
--
-- into
--
-- > ("hello" :> Get Hello) :<|> ("hello" :> ReqBody Hello :> Put Hello)
--
-- i.e distributing all ':>'-separated bits into the subsequent ':<|>'s.
type family Canonicalize (api :: k) :: k where
  -- requires UndecidableInstances
  Canonicalize ((a :: k) :> (b :<|> c)) = a :> Canonicalize b :<|> (Canonicalize (a :> c))
  Canonicalize ((a :<|> b) :> c)        = a :> Canonicalize c :<|> (Canonicalize (b :> c))
  Canonicalize ((a :: k) :> b)          = Redex b (Canonicalize b) a
  Canonicalize (a :<|> b)               = Canonicalize a :<|> Canonicalize b
  Canonicalize (a :: k)                 = a

type family Redex a b (c :: k) :: * where
  Redex a a first = Canonicalize first :> a
  Redex a b first = Canonicalize (first :> b)

canonicalize :: Proxy layout -> Proxy (Canonicalize layout)
canonicalize Proxy = Proxy
