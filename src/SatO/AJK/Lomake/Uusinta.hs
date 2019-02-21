{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module SatO.AJK.Lomake.Uusinta where

import Data.Generics.Labels ()
import Data.List.NonEmpty   (NonEmpty)
import Data.Reflection      (Given (..))
import Data.Semigroup       ((<>))
import Data.Text            (Text)
import Generics.SOP.TH      (deriveGeneric)
import GHC.Generics         (Generic)
import Network.Mail.Mime    (Address (..))

import Lomake

import SatO.AJK.Lomake.Classes
import SatO.AJK.Lomake.Common
import SatO.AJK.Lomake.LongText

import qualified Data.Text as T

data Person = Person
    { personFirstName  :: D "Etunimet"                        'Required Text
    , personCallName   :: D "Puhuttelunimi"                   'Optional Text
    , personLastName   :: D "Sukunimet"                       'Required Text
    , personBirthday   :: D "Syntymäaika"                     'Required Text
    , personBirthplace :: D "Syntymäpaikka"                   'Required Text
    , personAddress    :: D "Opiskelupaikkakunnan asunto-osoite" 'Required Text
    , personZipcode    :: D "Postinumero"                     'Required Text
    , personCity       :: D "Postitoimipaikka"                'Required Text
    , personEmail      :: D "Sähköpostiosoite"                'Required EmailText
    , personPhone      :: D "Puhelinnumero"                   'Required PhoneText
    , personSivilis    :: D "Siviilisääty"                    'Required Siv
    , personChildren   :: D "Alaikäiset lapset (syntymäajat)" 'Optional Text
    -- puolison nimi
    -- puolison ammatti
    }

data Studies = Studies
    { studiesYliopisto :: D "Yliopisto tai korkeakoulu, jossa opiskelet" 'Required Text
    , studiesTDK       :: D "Osasto, opintolinja tai tiedekunta ja koulutusohjelma" 'Required Text
    , studiesSemester  :: D "Tähänastiset läsnäololukukaudet"      'Required Text
    , studiesTime      :: D "Nykyisten opintojen alku-/päätösaika" 'Required Text
    , studiesUsed      :: D "Suoritettava tutkinto"                'Optional Text
    , studiesDone      :: D "Suoritetut korkeakoulututkinnot"      'Optional Text
    }

data Asumisoikeus = Asumisoikeus
    { asumisoikeusFirst     :: D "Koska muutit *ensimmäistä* kertaa Satalinnan Säätiön opiskelija-asuntoon?" 'Required Text
    , asumisoikeusIntervals :: D "Ajanjaksot, jolloin olet asumisoikeutesi aikana asunut *jossain muualla* kuin Satalinnan Säätiön opiskelija-asunnoissa" 'Required Text
    , asumisoikeusYears     :: D "Kuinka monta vuotta olet *yhteensä* asunut Satalinnan Säätiön opiskelija-asunnossa?" 'Required Text
    , asumisoikeusExtra     :: D "Lisää muut asumisoikeuteesi liittyvät asiat tähän" 'Optional LongText
    }

newtype Harrastuneisuus = Harrastuneisuus
    { overallHarrastuneisuus :: D "Osallistuminen Osakunnan/Säätiön tms. toimintaan" 'Optional LongText
    }

newtype Opintosuoritusote = Opintosuoritusote
    { getOpintosuritusote :: F "Opintosuoritusote"
    }
  deriving (Generic)

data Uusinta = Uusinta
    { uusintaPerson            :: D "Hakijan henkilötiedot" 'Required Person
    , uusintaStudies           :: D "Opinnot"               'Required Studies
    , uusintaAsumisoikeus      :: D "Asumisoikeus"          'Required Asumisoikeus
    , uusintaHarrastuneisuus   :: D "Harrastuneisuus"       'Required Harrastuneisuus
    , uusintaOpintosuoritusote :: D "Opintosuoritusote"     'Required Opintosuoritusote
    }
  deriving (Generic)

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

deriveGeneric ''Person
deriveGeneric ''Studies
deriveGeneric ''Asumisoikeus
deriveGeneric ''Harrastuneisuus
deriveGeneric ''Opintosuoritusote

deriveGeneric ''Uusinta

-------------------------------------------------------------------------------
-- Sections
-------------------------------------------------------------------------------

instance LomakeSection Person
instance LomakeSection Studies
instance LomakeSection Asumisoikeus
instance LomakeSection Harrastuneisuus
instance LomakeSection Opintosuoritusote

instance LomakeForm Uusinta where
    lomakeView     = sopFormView
    lomakeValidate = sopFormValidate
    lomakePretty   = sopFormPretty

    lomakePdfBS = #uusintaOpintosuoritusote . isoD . #getOpintosuritusote . isoF

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

instance LomakeName Uusinta where
    type LomakeShortName Uusinta = "uusiminen"

    lomakeTitle _      = "Asumisoikeuden uusiminen Satalinnan Säätiön asuntoihin"
    lomakeEmailTitle _ = "Asumisoikeuden uusiminen"
    lomakePreamble _   = Nothing
    lomakePdf _        = True

instance LomakeEmail Uusinta where
    lomakeSender ajk = name <> " " <> lastname
      where
        person :: Person
        person = unD $ uusintaPerson ajk

        -- | if there is callname, use it
        -- otherwise pick first of firstnames
        name :: Text
        name = case (unD . personCallName $ person, T.words . unD . personFirstName $ person) of
            (Just e, _ )     -> e
            (Nothing, [])    -> ""
            (Nothing, (x:_)) -> x

        lastname = unD (personLastName person)

    lomakeSend ajk  = Just $ Address (Just $ lomakeSender ajk) addr
      where
        addr = getFancyText . unD . personEmail . unD . uusintaPerson $ ajk

newtype UusintaAddress = UusintaAddress (NonEmpty Address)

instance Given UusintaAddress => LomakeAddress Uusinta where
    lomakeAddress _ = case given of
        UusintaAddress addr -> addr
