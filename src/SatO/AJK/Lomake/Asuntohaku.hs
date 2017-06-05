{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module SatO.AJK.Lomake.Asuntohaku where

import Data.List.NonEmpty (NonEmpty)
import Data.Reflection    (Given (..))
import Data.Semigroup     ((<>))
import Data.Text          (Text)
import Generics.SOP.TH    (deriveGeneric)
import Network.Mail.Mime  (Address (..))

import Lomake

import SatO.AJK.Lomake.Classes
import SatO.AJK.Lomake.LongText

import qualified Data.Text as T

data Siv = Naimaton | Aviossa
    deriving (Eq, Show, Enum, Bounded)

data Jasen = Varsinainen | Ulko
    deriving (Eq, Show, Enum, Bounded)

data Parents = BothAlive | OneAlive | Dead | YH
    deriving (Eq, Show, Enum, Bounded)

data Toistaiseksi = Toistaiseksi | Maaraikaiseksi
    deriving (Eq, Show, Enum, Bounded)

data Person = Person
    { personFirstName  :: D "Etunimet"                        'Required Text
    , personCallName   :: D "Puhuttelunimi"                   'Optional Text
    , personLastName   :: D "Sukunimet"                       'Required Text
    , personBirthday   :: D "Syntymäaika"                     'Required Text
    , personBirthplace :: D "Syntymäpaikka"                   'Required Text
    , personAddress    :: D "Postiosoite"                     'Required Text
    , personSivilis    :: D "Siviilisääty"                    'Required Siv
    , personEmail      :: D "Sähköpostiosoite"                'Required EmailText
    , personPhone      :: D' "Puhelinnumero"                  'Required PhoneText "Huom! Asukasvalinnoista ilmoitetaan puhelimitse."
    , personChildren   :: D "Alaikäiset lapset (syntymäajat)" 'Optional Text
    , personFacebook   :: D "Kotisivu tai sivu facebookissa"  'Optional Text
    }

data Studies = Studies
    { studiesYO        :: D "Ylioppilastutkinnon suoritusvuosi" 'Required Text
    , studiesLukio     :: D "Lukio"                             'Required Text
    , studiesYliopisto :: D "Yliopisto"                         'Required Text
    , studiesTDK       :: D "Tiedekunta ja koulutusohjelma"     'Required Text
    , studiesTime      :: D "Opintojen alku-/päätösaika"        'Required Text
    , studiesUsed      :: D "Suoritettava tutkinto"             'Optional Text
    , studiesDone      :: D "Suoritetut korkeakoulututkinnot"   'Optional Text
    }

data Talous = Talous
    { talousTulo        :: D "Verotettava tulo vuonna 2016"                                     'Required Text
    , talousVar         :: D "Verotettava varallisuus vuonna 2016"                              'Required Text
    , talousBrutto      :: D "Bruttotulot vuodelta 2016"                                        'Required Text
    , talousArvio       :: D "Arvio bruttotuloista vuonna 2017"                                 'Required Text
    , talousLaatu       :: D "Tulon laatu 2017 (ansio-, eläke-, pääoma-, vuokra- tai muu tulo)" 'Required Text
    , talousEmployer    :: D "Työnantaja vuonna 2017"                                           'Optional Text
    , talousPuolisoTulo :: D "Arvio puolison bruttotuloista vuonna 2017"                        'Optional Text
    , talousPuolisoJob  :: D "Puolison ammatti"                                                 'Optional Text
    , talousApuraha     :: D "Apurahat vuosina 2013-2017 (myöntäjä ja määrä)"                   'Optional Text
    }

data Family = Family
    { familyParents          :: D "Vanhemmista"                                                     'Required Parents
    , familyAlaSisar         :: D "Alaikäisten sisarusten lukumäärä"                                'Optional Text
    , familyDadJob           :: D "Isän ammatti"                                                    'Optional Text
    , familyMomJob           :: D "Äidin ammatti"                                                   'Optional Text
    , familyStudySisar       :: D "Opiskelevien täysi-ikäisten sisarusten lukumäärä"                'Optional Text
    , familySisarNames       :: D "Sisarusten etunimet"                                             'Optional Text
    , familyHkiAppartment    :: D "Omistatko tai omistavatko vanhempasi asunnon Helsingin seudulla" 'Required Bool
    , familuHkiAppartmentUse :: D "Onko asunto perheen omassa käytössä"                             'Optional Bool
    }

data Satakunta = Satakunta
    { satakuntaSiteet  :: D "Siteeni Satakuntaan (omin sanoin)"                                       'Required LongText
    , satakuntaFrom    :: D "Hakija on syntynyt, opiskellut tai asunut Satakunnassa"                  'Required Bool
    , satakuntaParents :: D "Hakijan vanhemmista jompikumpi on kuulunut Satakuntalaiseen osakuntaan"  'Required Bool
    -- , satakuntaTiedots :: D "Hakijan tietoja saa käyttää Satakuntalaisen Osakunnan tiedotukseen"      'Required Bool
    }

data Osakunta = Osakunta
    { osakuntaKirja :: D "Osakuntaan kirjoittautumisen lukukausi ja vuosi" 'Optional Text
    , osakuntaJasen :: D "Hakija on Satakuntalaisen osakunnan"             'Optional Jasen
    }

data OtherInfo = OtherInfo
    { otherInfoToistaiseksi :: D "Haen asuntoa:"                            'Required Toistaiseksi
    , otherInfoWhenEnd      :: D "Jos määräajaksi, niin mille aikavälille"  'Optional Text
    , otherInfoWhen         :: D "Milloin voin vastaanottaa asunnon"        'Required Text
    , otherInfoPet          :: D' "Mukanani muuttaa lemmikki"               'Required Bool
        "Huom: Satalinnan säätiön soluasunnoissa ei saa pitää lemmikkejä"
    }

data Overall = Overall
    { overallHobbies   :: D "Viisi tärkeintä harrastustani"                                          'Optional LongText
    , overallLuottamus :: D "Luottamustoimeni ja muu toimintani järjestöissä ja/tai yhdistyksissä"   'Optional LongText
    , overallBio       :: D "Luonnehdi halutessasi itseäsi (max 30 sanaa)"                           'Optional LongText
    , overallWhyYO     :: D "Miksi pyrit korkeakouluun opiskelemaan/miksi opiskelet korkeakoulussa"  'Optional LongText
    , overallWhyYou    :: D "Miksi juuri sinun pitäisi päästä asumaan Satalinnan Säätiön asuntoihin" 'Optional LongText
    }

data Asuntohaku = Asuntohaku
    { ajkPerson    :: D "Henkilötiedot"                   'Required Person
    , ajkStudies   :: D "Opinnot"                         'Required Studies
    , ajkTalous    :: D "Taloudellinen asema"             'Required Talous
    , ajkFamily    :: D "Perhesuhteet"                    'Required Family
    , ajkSatakunta :: D "Yhteydet Satakuntaan"            'Required Satakunta
    , ajkOsakunta  :: D "Ei koske opiskelunsa aloittavia" 'Required Osakunta
    , ajkOtherInfo :: D "Muita tietoja"                   'Required OtherInfo
    , ajkOverall   :: D "Yleistietoja"                    'Required Overall
    }

deriveGeneric ''Person
deriveGeneric ''Studies
deriveGeneric ''Talous
deriveGeneric ''Family
deriveGeneric ''Satakunta
deriveGeneric ''Osakunta
deriveGeneric ''OtherInfo
deriveGeneric ''Overall

deriveGeneric ''Asuntohaku

-------------------------------------------------------------------------------
-- Sections
-------------------------------------------------------------------------------

instance LomakeSection Person
instance LomakeSection Studies
instance LomakeSection Talous
instance LomakeSection Family
instance LomakeSection Satakunta
instance LomakeSection Osakunta
instance LomakeSection OtherInfo
instance LomakeSection Overall

instance LomakeForm Asuntohaku

-------------------------------------------------------------------------------
-- Siv
-------------------------------------------------------------------------------

instance HumanShow Siv where
    humanShow = T.pack . show

instance LomakeEnum Siv

instance LomakeField Siv where
    lomakeFieldView   = enumLomakeFieldView
    lomakeFieldValidate  = enumLomakeFieldValidate
    lomakeFieldPretty = ShortField . humanShow

-------------------------------------------------------------------------------
-- Parents
-------------------------------------------------------------------------------

instance HumanShow Parents where
    humanShow BothAlive = "Molemmat elävät"
    humanShow OneAlive  = "Vain toinen elää"
    humanShow Dead      = "Molemmat kuolleet"
    humanShow YH        = "Yh. äiti/isä"

instance LomakeEnum Parents

instance LomakeField Parents where
    lomakeFieldView   = enumLomakeFieldView
    lomakeFieldValidate  = enumLomakeFieldValidate
    lomakeFieldPretty = ShortField . humanShow

-------------------------------------------------------------------------------
-- Jasen
-------------------------------------------------------------------------------

instance HumanShow Jasen where
    humanShow Varsinainen  = "Varsinainen jäsen"
    humanShow Ulko         = "Ulkojäsen"

instance LomakeEnum Jasen

instance LomakeField Jasen where
    lomakeFieldView = enumLomakeFieldView
    lomakeFieldValidate = enumLomakeFieldValidate
    lomakeFieldPretty = ShortField . humanShow

-------------------------------------------------------------------------------
-- Toistaieksi
-------------------------------------------------------------------------------

instance HumanShow Toistaiseksi where
    humanShow Toistaiseksi   = "Toistaiseksi"
    humanShow Maaraikaiseksi = "Määräajaksi"

instance LomakeEnum Toistaiseksi

instance LomakeField Toistaiseksi where
    lomakeFieldView = enumLomakeFieldView
    lomakeFieldValidate = enumLomakeFieldValidate
    lomakeFieldPretty = ShortField . humanShow

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

instance LomakeName Asuntohaku where
    type LomakeShortName Asuntohaku = "ajk-lomake"
    lomakeTitle _ = "Hakulomake Satalinnan Säätion vuokraamiin huoneistoihin"
    lomakeEmailTitle _ = "Asuntohakemus"
    lomakePreamble _ = Just "Panostathan hakemukseen, sillä valinnat tehdään hakemusten perusteella."
    lomakePdf _ = True

instance LomakeEmail Asuntohaku where
    lomakeSender ajk = name <> " " <> lastname
      where
        person :: Person
        person = unD $ ajkPerson ajk

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
        addr = getFancyText . unD . personEmail . unD . ajkPerson $ ajk

newtype AsuntohakuAddress = AsuntohakuAddress (NonEmpty Address)

instance Given AsuntohakuAddress => LomakeAddress Asuntohaku where
    lomakeAddress _ = case given of
        AsuntohakuAddress addr -> addr
