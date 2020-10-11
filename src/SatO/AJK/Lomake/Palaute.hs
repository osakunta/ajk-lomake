{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module SatO.AJK.Lomake.Palaute where

import Data.List.NonEmpty     (NonEmpty)
import Data.Maybe             (fromMaybe)
import Data.Reflection        (Given (..))
import Data.Text              (Text)
import Generics.SOP.TH        (deriveGeneric)
import Network.SendGridV3.Api (MailAddress)

import Lomake

import SatO.AJK.Lomake.Classes
import SatO.AJK.Lomake.LongText

data Palaute' = Palaute'
    { palauteText   :: D "Palaute"           'Required LongText
    , palauteSource :: D "Palautteen antaja" 'Optional Text
    , palauteEmail  :: D "Sähköpostiosoite"  'Optional EmailText
    , palautePhone  :: D "Puhelinnumero"     'Optional PhoneText
    }

data Palaute = Palaute
    { palauteInner :: D ""  'Required Palaute'
    }

deriveGeneric ''Palaute'
deriveGeneric ''Palaute

instance LomakeSection Palaute'
instance LomakeForm Palaute

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

instance LomakeName Palaute where
    type LomakeShortName Palaute = "palautelomake"
    lomakeTitle _ = "Palautteen jättäminen asuntolajohtokunnalle"
    lomakeCompleted _ = "Palaute lähetetty."
    lomakeEmailTitle _ = "AJK Palaute"

instance LomakeEmail Palaute where
    lomakeSender sis = fromMaybe "anonyymi" $ unD (palauteSource inner)
      where
        inner :: Palaute'
        inner = unD $ palauteInner sis

newtype PalauteAddress = PalauteAddress (NonEmpty MailAddress)

instance Given PalauteAddress => LomakeAddress Palaute where
    lomakeAddress _ = case given of
        PalauteAddress addr -> addr

