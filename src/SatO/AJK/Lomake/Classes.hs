{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module SatO.AJK.Lomake.Classes where

import Data.Proxy        (Proxy (..))
import Data.Text         (Text, pack)
import GHC.TypeLits      (KnownSymbol, Symbol, symbolVal)
import Network.Mail.Mime (Address)

class KnownSymbol (LomakeShortName a) => LomakeName a where
    type LomakeShortName a :: Symbol
    lomakeTitle :: Proxy a -> Text

    lomakeShortName :: Proxy a -> Text
    lomakeShortName _ = pack $ symbolVal (Proxy :: Proxy (LomakeShortName a))

class LomakeEmail a where
    lomakeSender :: a -> Text

class LomakeAddress a where
    lomakeAddress :: Proxy a -> Address
