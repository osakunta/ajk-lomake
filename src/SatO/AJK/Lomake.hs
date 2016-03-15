{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module SatO.AJK.Lomake (
    defaultMain,
    Person(..),
    Studies(..),
    Talous(..),
    Family(..),
    Satakunta(..),
    Osakunta(..),
    OtherInfo(..),
    Overall(..),
    AJK(..),
    IndexPage(..),
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.FileEmbed         (embedStringFile)
import Data.Semigroup         ((<>))
import Data.String            (fromString)
import Data.Text              (Text)
import Generics.SOP.TH        (deriveGeneric)
import Lucid
import Network.Wai
import Servant
import Servant.HTML.Lucid
import System.IO              (hPutStrLn, stderr)

import qualified Network.Wai.Handler.Warp as Warp

import Lomake

data Siv = Naimaton | Aviossa
    deriving (Eq, Show, Enum, Bounded)

data Jasen = Varsinainen | Ulko
    deriving (Eq, Show, Enum, Bounded)

data Parents = BothAlive | OneAlive | Dead | YH
    deriving (Eq, Show, Enum, Bounded)

newtype LongText = LongText Text

data Person = Person
    { personFirstName  :: D "Etunimet (puhuttelunimi ensin)"  'True  Text
    , personLastName   :: D "Sukunimet"                       'True  Text
    , personBirthday   :: D "Syntymäaika"                     'True  Text
    , personBirthplace :: D "Syntymäpaikka"                   'True  Text
    , personAddress    :: D "Postiosoite"                     'True  Text
    , personSivilis    :: D "Siviilisääty"                    'True  Siv
    , personEmail      :: D "Sähköpostiosoite"                'True  Text
    , personChildren   :: D "Alaikäiset lapset (syntymäajat)" 'False Text
    , personFacebook   :: D "Kotisivu tai sivu facebookissa"  'False Text
    }

data Studies = Studies
    { studiesYO        :: D "Ylioppilastutkinnon suoritusvuosi" 'True  Text
    , studiesLukio     :: D "Lukio"                             'True  Text
    , studiesYliopisto :: D "Yliopisto"                         'True  Text
    , studiesTDK       :: D "Tiedekunta ja koulutusohjelma"     'True  Text
    , studiesTime      :: D "Opintojen alku-/päätösaika"        'True  Text
    , studiesUsed      :: D "Suoritettava tutkinto"             'False Text
    , studiesDone      :: D "Suoritetut korkeakoulututkinnot"   'False Text
    }

data Talous = Talous
    { talousTulo        :: D "Verotettava tulo vuonna 2014"                                     'True  Text
    , talousVar         :: D "Verotettava varallisuus vuonna 2014"                              'True  Text
    , talousBrutto      :: D "Bruttotulot vuodelta 2014"                                        'True  Text
    , talousArvio       :: D "Arvio bruttotuloista vuonna 2015"                                 'True  Text
    , talousLaatu       :: D "Tulon laatu 2015 (ansio-, eläke-, pääoma-, vuokra- tai muu tulo)" 'True  Text
    , talousEmployer    :: D "Työnantaja vuonna 2015"                                           'False Text
    , talousPuolisoTulo :: D "Arvio puolison bruttotuloista vuonna 2015"                        'False Text
    , talousPuolisoJob  :: D "Puolison ammatti"                                                 'False Text
    , talousApuraha     :: D "Apurahat vuosina 2012-2015 (myöntäjä ja määrä)"                   'False Text
    }

data Family = Family
    { familyParents          :: D "Vanhemmista"                                                     'True  Parents
    , familyAlaSisar         :: D "Alaikäisten sisarusten lukumäärä"                                'False Text
    , familyDadJob           :: D "Isän ammatti"                                                    'False Text
    , familyMomJob           :: D "Äidin ammatti"                                                   'False Text
    , familyStudySisar       :: D "Opiskelevien täysi-ikäisten sisarusten lukumäärä"                'False Text
    , familySisarNames       :: D "Sisarusten etunimet"                                             'False Text
    , familyHkiAppartment    :: D "Omistatko tai omistavatko vanhempasi asunnon Helsingin seudulla" 'True  Bool
    , familuHkiAppartmentUse :: D "Onko asunto perheen omassa käytössä"                             'False Bool
    }

data Satakunta = Satakunta
    { satakuntaSiteet  :: D "Siteeni Satakuntaan (omin sanoin)"                                       'True LongText
    , satakuntaFrom    :: D "Hakija on syntynyt, opiskellut tai asunut Satakunnassa"                  'True Bool
    , satakuntaParents :: D "Hakijan vanhemmista jompikumpi on kuulunut Satakuntalaiseen osakuntaan" 'True Bool
    , satakuntaTiedots :: D "Hakijan tietoja saa käyttää Satakuntalaisen Osakunnan tiedotukseen"      'True Bool
    }

data Osakunta = Osakunta
    { osakuntaKirja :: D "Osakuntaan kirjoittautumisen lukukausi ja vuosi" 'True  Text
    , osakuntaJasen :: D "Hakija on Satakuntalaisen osakunnan"             'False Jasen
    }

data OtherInfo = OtherInfo
    { otherInfoWhen :: D "Milloin voin vastaanottaa asunnon" 'True Text
    }

data Overall = Overall
    { overallHobbies   :: D "Viisi tärkeintä harrastustani"                                          'False LongText
    , overallLuottamus :: D "Luottamustoimeni ja muu toimintani järjestöissä ja/tai yhdistyksissä"   'False LongText
    , overallBio       :: D "Luonnehdi halutessasi itseäsi (max 30 sanaa)"                           'False LongText
    , overallWhyYO     :: D "Miksi pyrit korkeakouluun opiskelemaan/miksi opiskelet korkeakoulussa"  'False LongText
    , overallWhyYou    :: D "Miksi juuri sinun pitäisi päästä asumaan Satalinnan Säätiön asuntoihin" 'False LongText
    }

data AJK = AJK
    { ajkPerson    :: D "Henkilötiedot"                   'True Person
    , ajkStudies   :: D "Opinnot"                         'True Studies
    , ajkTalous    :: D "Taloudellinen asema"             'True Talous
    , ajkFamily    :: D "Perhesuhteet"                    'True Family
    , ajkSatakunta :: D "Yhteydet Satakuntaan"            'True Satakunta
    , ajkOsakunta  :: D "Ei koske opiskelunsa aloittavia" 'True Osakunta
    , ajkOtherInfo :: D "Muita tietoja"                   'True OtherInfo
    , ajkOverall   :: D "Yleistietoja"                    'True Overall
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

-------------------------------------------------------------------------------
-- LongText
-------------------------------------------------------------------------------

instance LomakeField LongText where
    lomakeFieldView _ env name =
       textarea_ [name_ name] $ toHtml $ submittedTextValue env name
    lomakeFieldPretty (LongText t) = "\n" <> lomakeFieldPretty t <> "\n"
    lomakeFieldValidate _proxyA proxyReq = ofmap proxyReq LongText . lomakeText proxyReq

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

newtype IndexPage = IndexPage (LomakeResult AJK)
newtype ConfirmPage = ConfirmPage Bool -- Error

type AJKLomakeAPI =
    Get '[HTML] IndexPage
    :<|> ReqBody '[FormUrlEncoded] (LomakeResult AJK) :> Post '[HTML] IndexPage
    :<|> "send" :> ReqBody '[FormUrlEncoded] (LomakeResult AJK) :> Post '[HTML] ConfirmPage

instance ToHtml IndexPage where
    toHtml (IndexPage (LomakeResult env v)) = page_ "Asuntohaku Satalinnan säätiön asuntoihin" $ do
        case v of
            Nothing -> do
                form_ [action_ actionUrl, method_ "POST"] $ do
                    lomakeView (Proxy :: Proxy AJK) env
                    hr_ []
                    input_ [type_ "submit", value_ "Esikatsele"]
                    toHtmlRaw ("&nbsp;" :: Text)
                    input_ [type_ "reset", value_ "Tyhjennä"]
            Just ajk -> do
                h2_ $ "Tarkista tietosi vielä kerran:"
                pre_ $ toHtml $ render $ lomakePretty ajk
                hr_ []
                form_ [action_ $ actionUrl <> "send", method_ "POST"] $ do
                    hiddenForm env
                    input_ [type_ "submit", value_ "Lähetä"]
    toHtmlRaw _ = pure ()

instance ToHtml ConfirmPage where
    toHtml (ConfirmPage sent) = doctypehtml_ $ do
        head_ $ do
            style_ $ fromString $ $(embedStringFile "style.css")
            title_ "Kiitos hakemuksestasi Satalinnan säätiön asuntoihin"
        body_ $ do
            case sent of
                True  -> div_ $ "Kiitos hakemuksestasi!"
                False -> div_ $ "Virhe! Jotain odottamatonta tapahtui. Kokeile hetken päästä uudestaan."
    toHtmlRaw _ = pure ()

ajkLomakeApi :: Proxy AJKLomakeAPI
ajkLomakeApi = Proxy

firstPost :: MonadIO m => LomakeResult AJK -> m IndexPage
firstPost = return . IndexPage

secondPost :: MonadIO m => LomakeResult AJK -> m ConfirmPage
secondPost (LomakeResult _ Nothing) = pure $ ConfirmPage False
secondPost (LomakeResult _ (Just _ajk)) = do
    -- TODO: send email
    pure $ ConfirmPage True

actionUrl :: Text
actionUrl = "/"

-------------------------------------------------------------------------------
-- HTML stuff
-------------------------------------------------------------------------------

-- | Page template.
page_ :: Monad m => Text -> HtmlT m () -> HtmlT m ()
page_ t b = doctypehtml_ $ do
    head_ $ do
        title_ $ toHtml t
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        meta_ [httpEquiv_ "x-ua-compatible", content_"ie=edge"]
        style_ [type_ "text/css"] ($(embedStringFile "foundation-6/css/foundation.min.css") :: String)
        style_ [type_ "text/css"] ($(embedStringFile "style.css") :: String)
    body_ $ do
        b

-------------------------------------------------------------------------------
-- WAI boilerplate
-------------------------------------------------------------------------------

server :: Server AJKLomakeAPI
server = pure (IndexPage (LomakeResult emptyLomakeEnv Nothing))
    :<|> firstPost
    :<|> secondPost

app :: Application
app = serve ajkLomakeApi server

defaultMain :: IO ()
defaultMain = do
    hPutStrLn stderr "Hello, ajk-lomake-api is alive"
    hPutStrLn stderr "Starting web server"
    Warp.run 8080 app
