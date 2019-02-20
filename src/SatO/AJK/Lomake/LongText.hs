{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module SatO.AJK.Lomake.LongText where

import Data.Char            (isDigit)
import Data.Proxy           (Proxy (..))
import Data.Singletons.Bool
import Data.Text            (pack)
import Data.Type.Equality
import Futurice.Prelude
import GHC.TypeLits         (KnownSymbol, Symbol, symbolVal)
import Lomake
import Lucid
import Prelude ()

import qualified Data.Text as T

newtype LongText = LongText Text
instance LomakeField LongText where
    lomakeFieldView _ env name =
       textarea_ [name_ name] $ toHtml $ submittedTextValue env name
    lomakeFieldPretty (LongText t) = LongField t
    lomakeFieldValidate _proxyA proxyReq = (fmap . ofmap proxyReq) LongText . lomakeText proxyReq

newtype FancyText (sym :: Symbol) = FancyText { getFancyText :: Text }

type EmailText = FancyText "email"
type PhoneText = FancyText "tel"

instance
    ( KnownSymbol sym
    , SBoolI btel, btel ~ (sym == "tel")
    , SBoolI bema, bema ~ (sym == "email")
    )
    => LomakeField (FancyText sym)
  where
    lomakeFieldView _ env name =
        input_ [type_ t, name_  name, value_ $ submittedTextValue env name]
      where
        t = pack $ symbolVal (Proxy :: Proxy sym)
    lomakeFieldPretty = ShortField . getFancyText

    lomakeFieldValidate _ p name =
        ovalidate p v $ lomakeText p name
      where
        v :: Text -> Either (Text, Text) (FancyText sym)
        v = case (sbool :: SBool btel, sbool :: SBool bema) of
            (SFalse, SFalse) -> pure . (FancyText :: Text -> FancyText sym)
            (STrue , _     ) -> predPhone
            (_     , STrue ) -> predEmail

        validPhone :: Text -> Bool
        validPhone = (> 7) . T.length . T.filter isDigit

        predPhone :: Text -> Either (Text, Text) (FancyText sym)
        predPhone x | validPhone x = Right (FancyText x)
                    | otherwise    = Left (name, "Liian vähän numeroita puhelinnumerossa")

        validEmail :: Text -> Bool
        validEmail = T.any (== '@')

        predEmail :: Text -> Either (Text, Text) (FancyText sym)
        predEmail x | validEmail x = Right (FancyText x)
                    | otherwise    = Left (name, "Sähköpostiosoitteessa pitää olla @-merkki")
