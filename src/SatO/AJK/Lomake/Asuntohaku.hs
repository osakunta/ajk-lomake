{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}
module SatO.AJK.Lomake.Asuntohaku where

import Data.Text                 (Text)
import Generics.SOP.TH           (deriveGeneric)

import Lomake

import SatO.AJK.Lomake.LongText

data Siv = Naimaton | Aviossa
    deriving (Eq, Show, Enum, Bounded)

data Jasen = Varsinainen | Ulko
    deriving (Eq, Show, Enum, Bounded)

data Parents = BothAlive | OneAlive | Dead | YH
    deriving (Eq, Show, Enum, Bounded)


data Person = Person
    { personFirstName  :: D "Etunimet"                        'Required Text
    , personCallName   :: D "Puhuttelunimi"                   'Optional Text
    , personLastName   :: D "Sukunimet"                       'Required Text
    , personBirthday   :: D "Syntymäaika"                     'Required Text
    , personBirthplace :: D "Syntymäpaikka"                   'Required Text
    , personAddress    :: D "Postiosoite"                     'Required Text
    , personSivilis    :: D "Siviilisääty"                    'Required Siv
    , personEmail      :: D "Sähköpostiosoite"                'Required Text
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
    { talousTulo        :: D "Verotettava tulo vuonna 2014"                                     'Required Text
    , talousVar         :: D "Verotettava varallisuus vuonna 2014"                              'Required Text
    , talousBrutto      :: D "Bruttotulot vuodelta 2014"                                        'Required Text
    , talousArvio       :: D "Arvio bruttotuloista vuonna 2015"                                 'Required Text
    , talousLaatu       :: D "Tulon laatu 2015 (ansio-, eläke-, pääoma-, vuokra- tai muu tulo)" 'Required Text
    , talousEmployer    :: D "Työnantaja vuonna 2015"                                           'Optional Text
    , talousPuolisoTulo :: D "Arvio puolison bruttotuloista vuonna 2015"                        'Optional Text
    , talousPuolisoJob  :: D "Puolison ammatti"                                                 'Optional Text
    , talousApuraha     :: D "Apurahat vuosina 2012-2015 (myöntäjä ja määrä)"                   'Optional Text
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
    , satakuntaTiedots :: D "Hakijan tietoja saa käyttää Satakuntalaisen Osakunnan tiedotukseen"      'Required Bool
    }

data Osakunta = Osakunta
    { osakuntaKirja :: D "Osakuntaan kirjoittautumisen lukukausi ja vuosi" 'Required Text
    , osakuntaJasen :: D "Hakija on Satakuntalaisen osakunnan"             'Optional Jasen
    }

data OtherInfo = OtherInfo
    { otherInfoWhen :: D "Milloin voin vastaanottaa asunnon" 'Required Text
    }

data Overall = Overall
    { overallHobbies   :: D "Viisi tärkeintä harrastustani"                                          'Optional LongText
    , overallLuottamus :: D "Luottamustoimeni ja muu toimintani järjestöissä ja/tai yhdistyksissä"   'Optional LongText
    , overallBio       :: D "Luonnehdi halutessasi itseäsi (max 30 sanaa)"                           'Optional LongText
    , overallWhyYO     :: D "Miksi pyrit korkeakouluun opiskelemaan/miksi opiskelet korkeakoulussa"  'Optional LongText
    , overallWhyYou    :: D "Miksi juuri sinun pitäisi päästä asumaan Satalinnan Säätiön asuntoihin" 'Optional LongText
    }

data AJK = AJK
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

deriveGeneric ''AJK

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

instance LomakeForm AJK

-------------------------------------------------------------------------------
-- Siv
-------------------------------------------------------------------------------

instance HumanShow Siv where
    humanShow = show

instance LomakeEnum Siv

instance LomakeField Siv where
    lomakeFieldView   = enumLomakeFieldView
    lomakeFieldValidate  = enumLomakeFieldValidate
    lomakeFieldPretty = text . humanShow

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
    lomakeFieldPretty = text . humanShow

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
    lomakeFieldPretty = text . humanShow
