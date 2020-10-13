{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module SatO.AJK.Lomake.Classes where

import Data.List.NonEmpty     (NonEmpty)
import Data.Proxy             (Proxy (..))
import Data.Text              (Text, pack)
import GHC.TypeLits           (KnownSymbol, Symbol, symbolVal)
import Network.SendGridV3.Api (MailAddress)

class KnownSymbol (LomakeShortName a) => LomakeName a where
    type LomakeShortName a :: Symbol
    lomakeTitle :: Proxy a -> Text

    lomakeShortName :: Proxy a -> Text
    lomakeShortName _ = pack $ symbolVal (Proxy :: Proxy (LomakeShortName a))

    lomakePreamble :: Proxy a -> Maybe Text
    lomakePreamble _ = Nothing

    lomakeCompleted :: Proxy a -> Text
    lomakeCompleted _ = "Hakemus lähetetty. Kiitos hakemuksestasi!"

    lomakeEmailTitle :: Proxy a -> Text

    lomakeRedo :: Proxy a -> Bool
    lomakeRedo _ = False

    lomakePdf :: Proxy a -> Bool
    lomakePdf _ = False

class LomakeEmail a where
    lomakeSender :: a -> Text

    -- | Send lomake to applicant
    lomakeSend :: a -> Maybe MailAddress
    lomakeSend _ = Nothing

class LomakeAddress a where
    lomakeAddress :: Proxy a -> NonEmpty MailAddress
