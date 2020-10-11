{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module SatO.AJK.Lomake.Huoltoilmoitus where

import Data.List.NonEmpty     (NonEmpty)
import Data.Reflection        (Given (..))
import Data.Semigroup         ((<>))
import Data.Text              (Text)
import Generics.SOP.TH        (deriveGeneric)
import Network.SendGridV3.Api (MailAddress)

import Lomake

import SatO.AJK.Lomake.Classes
import SatO.AJK.Lomake.LongText

data Huoltoilmoitus' = Huoltoilmoitus'
    { huoltoKohde  :: D "Huoltokohde"           'Required Text
    , huoltoVika   :: D "Vika"                  'Required LongText
    , huoltoYleis  :: D "Vian saa tulla korjaamaan huoneeseen yleisavaimella päiväsaikaan. Korjaus tapahtuu nopeammin, jos huoltomies saa käydä vapaasti korjaamassa vian." 'Required Bool
      -- ^ Huoltomiehen puhelinnumero on 050-2861.
    , huoltoSource :: D "Ilmoituksen tekijä"    'Required Text
    , huoltoEmail  :: D "Sähköpostiosoite"      'Required EmailText
    , huoltoPhone  :: D "Puhelinnumero"         'Required PhoneText
    }

data Huoltoilmoitus = Huoltoilmoitus
    { huoltoInner :: D "Tiedot"  'Required Huoltoilmoitus'
    }

deriveGeneric ''Huoltoilmoitus'
deriveGeneric ''Huoltoilmoitus

instance LomakeSection Huoltoilmoitus'
instance LomakeForm Huoltoilmoitus

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

instance LomakeName Huoltoilmoitus where
    type LomakeShortName Huoltoilmoitus = "huoltoilmoitus"
    lomakeTitle _ = "Satalinnan Säätio - huoltoilmoitus"
    lomakeCompleted _ = "Huoltoilmoitus lähetetty."
    lomakeEmailTitle _ = "Huoltoilmoitus"
    lomakeRedo _ = True

instance LomakeEmail Huoltoilmoitus where
    lomakeSender sis = unD (huoltoKohde inner) <> " " <> unD (huoltoSource inner)
      where
        inner :: Huoltoilmoitus'
        inner = unD $ huoltoInner sis

newtype HuoltoilmoitusAddress = HuoltoilmoitusAddress (NonEmpty MailAddress)

instance Given HuoltoilmoitusAddress => LomakeAddress Huoltoilmoitus where
    lomakeAddress _ = case given of
        HuoltoilmoitusAddress addr -> addr

