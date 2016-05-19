{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module SatO.AJK.Lomake.Sisanen where

import Data.Reflection   (Given (..))
import Data.Semigroup    ((<>))
import Data.Text         (Text)
import Generics.SOP.TH   (deriveGeneric)
import Network.Mail.Mime (Address)

import Lomake

import SatO.AJK.Lomake.Classes
import SatO.AJK.Lomake.LongText

data SisPerson = SisPerson
    { sisFirstName :: D "Etunimet"         'Required Text
    , sisLastName  :: D "Sukunimet"        'Required Text
    , sisEmail     :: D "Sähköpostiosoite" 'Required Text
    , sisPhone     :: D "Puhelinnumero"    'Required Text
    }

data SisAsunto = SisAsunto
    { sisWhich    :: D "Haettava asunto"                                           'Required Text
    , sisReason   :: D "Miksi haet kyseistä asuntoa"                               'Required LongText
    , sisOther    :: D "Olen kiinnostunut myös muista vapautuvista asunnoista"     'Required Bool
    , sisSize     :: D "Jos kyllä, niin mikä on muiden asuntojen minimikoko (m2)"  'Optional Text
    , sisHistory  :: D "Asumishistoria Satalinnan säätiön asuntolassa"             'Required LongText
    , sisActivity :: D "Toiminta osakunnalla ja/tai säätiössä"                     'Required LongText
    }

data Sisanen = Sisanen
    { sisPerson :: D "Henkilötiedot"          'Required SisPerson
    , sisAsunto :: D "Asunto- ja muut tiedot" 'Required SisAsunto
    }

deriveGeneric ''SisPerson
deriveGeneric ''SisAsunto

deriveGeneric ''Sisanen

instance LomakeSection SisPerson
instance LomakeSection SisAsunto

instance LomakeForm Sisanen

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

instance LomakeName Sisanen where
    type LomakeShortName Sisanen = "sisanen-haku"
    lomakeTitle _ = "Satalinnan Säätion sisäinen asuntohaku"

instance LomakeEmail Sisanen where
    lomakeSender sis = unD (sisFirstName person) <> " " <> unD (sisLastName person)
      where
        person :: SisPerson
        person = unD $ sisPerson sis

newtype SisanenAddress = SisanenAddress Address

instance Given SisanenAddress => LomakeAddress Sisanen where
    lomakeAddress _ = case given of
        SisanenAddress addr -> addr
