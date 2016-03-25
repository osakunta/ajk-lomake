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
import Data.Maybe             (fromMaybe)
import Data.Semigroup         ((<>))
import Data.Text              (Text)
import Generics.SOP.TH        (deriveGeneric)
import Lucid
import Network.Wai
import Servant
import Servant.HTML.Lucid
import Network.Mail.Mime
import System.Environment     (lookupEnv)
import System.IO              (hPutStrLn, stderr, stdout)
import Text.Read              (readMaybe)

import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
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
    { personFirstName  :: D "Etunimet (puhuttelunimi ensin)"  'Required Text
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

type ActionUrl = Text 

data IndexPage = IndexPage
    { _indexPageActionUrl :: !ActionUrl
    , _indexPageAjk       :: !(LomakeResult AJK)
    }
newtype ConfirmPage = ConfirmPage Bool -- Error

data Ctx = Ctx
    { _ctxActionUrl :: !ActionUrl
    , _ctxToAddress :: !Address
    }

type AJKLomakeAPI =
    Get '[HTML] IndexPage
    :<|> ReqBody '[FormUrlEncoded] (LomakeResult AJK) :> Post '[HTML] IndexPage
    :<|> "send" :> ReqBody '[FormUrlEncoded] (LomakeResult AJK) :> Post '[HTML] ConfirmPage

instance ToHtml IndexPage where
    toHtml (IndexPage actionUrl (LomakeResult env v)) = page_ "Hakulomake Satalinnan Säätion vuokraamiin huoneistoihin" $ do
        case v of
            Nothing -> do
                form_ [action_ actionUrl, method_ "POST"] $ do
                    div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
                        h1_ $ "Hakulomake Satalinnan Säätion vuokraamiin huoneistoihin"
                    lomakeView (Proxy :: Proxy AJK) env
                    div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
                        input_ [class_ "medium success button", type_ "submit", value_ "Esikatsele"]
            Just ajk -> do
                div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
                    h2_ $ "Tarkista tietosi vielä kerran:"
                div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
                    pre_ $ toHtml $ render $ lomakePretty ajk
                hr_ []
                form_ [action_ $ actionUrl <> "send", method_ "POST"] $ do
                    hiddenForm env
                    div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
                        input_ [class_ "medium success button", type_ "submit", value_ "Lähetä"]
    toHtmlRaw _ = pure ()

instance ToHtml ConfirmPage where
    toHtml (ConfirmPage sent) = page_ "Asuntohaku Satalinnan säätiön aesuntoihin" $ do
        div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
            case sent of
                True  -> div_ $ "Kiitos hakemuksestasi!"
                False -> div_ $ "Virhe! Jotain odottamatonta tapahtui. Kokeile hetken päästä uudestaan."
    toHtmlRaw _ = pure ()

ajkLomakeApi :: Proxy AJKLomakeAPI
ajkLomakeApi = Proxy

firstPost :: MonadIO m => Ctx -> LomakeResult AJK -> m IndexPage
firstPost (Ctx actionUrl _) = return . IndexPage actionUrl

secondPost :: MonadIO m => Ctx -> LomakeResult AJK -> m ConfirmPage
secondPost _         (LomakeResult _ Nothing) = pure $ ConfirmPage False
secondPost (Ctx _ a) (LomakeResult _ (Just ajk)) = do
    liftIO $ do
        hPutStrLn stderr $ "Sending application from " <> T.unpack name <> " to " <> show a
        hPutStrLn stdout $ TL.unpack body
        bs <- renderMail' mail
        sendmail bs
    pure $ ConfirmPage True
  where
    mail :: Mail
    mail = simpleMail' a fromAddress subject body

    body :: TL.Text
    body = TL.fromStrict $ T.pack $ render $ lomakePretty ajk

    subject :: Text
    subject = "Asuntohakemus " <> name

    name :: Text
    name = unD (personFirstName person) <> " " <> unD (personLastName person)
      where
        person :: Person
        person = unD $ ajkPerson ajk

    fromAddress :: Address
    fromAddress = Address (Just "AJK-Lomake") "ajk-lomake@satakuntatalo.fi"

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

server :: Ctx -> Server AJKLomakeAPI
server ctx@(Ctx actionUrl _) = pure (IndexPage actionUrl (LomakeResult emptyLomakeEnv Nothing))
    :<|> firstPost ctx
    :<|> secondPost ctx

app :: Ctx -> Application
app ctx = serve ajkLomakeApi (server ctx)

lookupEnvWithDefault :: Read a => a -> String -> IO a
lookupEnvWithDefault def v = do
    x <- lookupEnv v
    return $ fromMaybe def (x >>= readMaybe)

defaultMain :: IO ()
defaultMain = do
    port <- lookupEnvWithDefault 8080 "PORT"
    emailAddr <- fromMaybe "foo@example.com" <$> lookupEnv "LOMAKE_EMAILADDR"
    actionUrl <- fromMaybe "/"               <$> lookupEnv "LOMAKE_ACTIONURL"
    let ctx = Ctx (T.pack actionUrl) (Address Nothing $ T.pack emailAddr)
    hPutStrLn stderr "Hello, ajk-lomake-api is alive"
    hPutStrLn stderr "Starting web server"
    Warp.run port (app ctx)
