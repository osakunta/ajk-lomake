{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SatO.AJK.Lomake.LongText where

import Data.Proxy     (Proxy (..))
import Data.Semigroup ((<>))
import Data.Text      (Text, pack, unpack)
import GHC.TypeLits   (KnownSymbol, Symbol, symbolVal)
import Lomake
import Lucid

newtype LongText = LongText Text
instance LomakeField LongText where
    lomakeFieldView _ env name =
       textarea_ [name_ name] $ toHtml $ submittedTextValue env name
    lomakeFieldPretty (LongText t) = "\n" <> lomakeFieldPretty t <> "\n"
    lomakeFieldValidate _proxyA proxyReq = ofmap proxyReq LongText . lomakeText proxyReq

newtype FancyText (sym :: Symbol) = FancyText { getFancyText :: Text }

type EmailText = FancyText "email"
type PhoneText = FancyText "tel"

instance KnownSymbol sym => LomakeField (FancyText sym) where
    lomakeFieldView _ env name =
        input_ [type_ t, name_  name, value_ $ submittedTextValue env name]
      where
        t = pack $ symbolVal (Proxy :: Proxy sym)
    lomakeFieldPretty = text . unpack . getFancyText
    lomakeFieldValidate _ p t =
        ofmap p (FancyText :: Text -> FancyText sym) $ lomakeText p t
