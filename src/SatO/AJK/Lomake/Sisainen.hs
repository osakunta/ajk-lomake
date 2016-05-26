{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module SatO.AJK.Lomake.Sisainen where

import Data.List.NonEmpty (NonEmpty)
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
    , sisLastName  :: D "Sukunimi"         'Required Text
    , sisEmail     :: D "Sähköpostiosoite" 'Required EmailText
    , sisPhone     :: D "Puhelinnumero"    'Required PhoneText
    }

data SisAsunto = SisAsunto
    { sisWhich    :: D "Haettava asunto"                                           'Required Text
    , sisReason   :: D "Miksi haet kyseistä asuntoa"                               'Required LongText
    , sisOther    :: D "Olen kiinnostunut myös muista vapautuvista asunnoista"     'Required Bool
    , sisSize     :: D "Jos kyllä, niin mikä on muiden asuntojen minimikoko (m2) ja muut vaatimukset" 
                                                                                   'Optional Text
    , sisHistory  :: D "Asumishistoria Satalinnan säätiön asuntolassa"             'Required LongText
    , sisActivity :: D "Toiminta osakunnalla ja/tai säätiössä"                     'Required LongText
    , sisFree     :: D "Muut perustelut"                                           'Optional LongText
    }

data Sisainen = Sisainen
    { sisPerson :: D "Henkilötiedot"          'Required SisPerson
    , sisAsunto :: D "Asunto- ja muut tiedot" 'Required SisAsunto
    }

deriveGeneric ''SisPerson
deriveGeneric ''SisAsunto

deriveGeneric ''Sisainen

instance LomakeSection SisPerson
instance LomakeSection SisAsunto

instance LomakeForm Sisainen

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

instance LomakeName Sisainen where
    type LomakeShortName Sisainen = "sisainen-haku"
    lomakeTitle _ = "Satalinnan Säätion sisäinen asuntohaku"
    lomakePreamble _ = Just "Kaikki hakemisen kannalta olennainen kannattaa mainita. Eniten valintaan vaikuttaa aktiivisuus osakunnalla ja säätiössä. Kaikki tiedot käsitellään luottamuksellisesti."

instance LomakeEmail Sisainen where
    lomakeSender sis = unD (sisFirstName person) <> " " <> unD (sisLastName person)
      where
        person :: SisPerson
        person = unD $ sisPerson sis

newtype SisainenAddress = SisainenAddress (NonEmpty Address)

instance Given SisainenAddress => LomakeAddress Sisainen where
    lomakeAddress _ = case given of
        SisainenAddress addr -> addr
