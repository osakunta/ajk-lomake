{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TemplateHaskell     #-}
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

import Control.Exception         (SomeException)
import Control.Monad.IO.Class    (MonadIO (..))
import Data.FileEmbed            (embedStringFile)
import Data.Function             ((&))
import Data.Maybe                (fromMaybe)
import Data.Semigroup            ((<>))
import Data.String               (fromString)
import Data.Text                 (Text)
import Lucid
import Network.HTTP.Types.Status (status500)
import Network.Mail.Mime
import Network.Wai
import Servant
import Servant.HTML.Lucid
import System.Environment        (lookupEnv)
import System.IO                 (hPutStrLn, stderr, stdout)
import Text.Read                 (readMaybe)

import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import qualified Network.Wai.Handler.Warp as Warp

import Lomake

import SatO.AJK.Lomake.Asuntohaku
import SatO.AJK.Lomake.Sisanen

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

type ActionUrl = Text

data IndexPage = IndexPage
    { _indexPageActionUrl :: !ActionUrl
    , _indexPageAjk       :: !(LomakeResult AJK)
    }
newtype ConfirmPage = ConfirmPage Bool -- Error

data SisanenPage = SisanenPage
    { _sisPageActionUrl :: !ActionUrl
    , _sisPageAjk       :: !(LomakeResult Sisanen)
    }

data Ctx = Ctx
    { _ctxActionUrl :: !ActionUrl
    , _ctxToAddress :: !Address
    }

type AJKLomakeAPI =
    "ajk-lomake" :> Get '[HTML] IndexPage
    :<|> "ajk-lomake" :> ReqBody '[FormUrlEncoded] (LomakeResult AJK) :> Post '[HTML] IndexPage
    :<|> "ajk-lomake" :> "send" :> ReqBody '[FormUrlEncoded] (LomakeResult AJK) :> Post '[HTML] ConfirmPage
    :<|> "sisanen-haku" :> Get '[HTML] SisanenPage
    :<|> "sisanen-haku" :> ReqBody '[FormUrlEncoded] (LomakeResult Sisanen) :> Post '[HTML] SisanenPage

instance ToHtml IndexPage where
    toHtmlRaw _ = pure ()
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

instance ToHtml SisanenPage where
    toHtmlRaw _ = pure ()
    toHtml (SisanenPage actionUrl (LomakeResult env v)) = page_ "Hakulomake Satalinnan Säätion vuokraamiin huoneistoihin" $ do
        case v of
            Nothing -> do
                form_ [action_ $ actionUrl <> "sisanen-haku", method_ "POST"] $ do
                    div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
                        h1_ $ "Hakulomake Satalinnan Säätion vuokraamiin huoneistoihin"
                    lomakeView (Proxy :: Proxy Sisanen) env
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

instance ToHtml ConfirmPage where
    toHtmlRaw _ = pure ()
    toHtml (ConfirmPage sent) = page_ "Asuntohaku Satalinnan säätiön aesuntoihin" $ do
        div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
            case sent of
                True  -> div_ $ "Kiitos hakemuksestasi!"
                False -> div_ $ "Virhe! Jotain odottamatonta tapahtui. Kokeile hetken päästä uudestaan."

ajkLomakeApi :: Proxy AJKLomakeAPI
ajkLomakeApi = Proxy

firstPost :: MonadIO m => Ctx -> LomakeResult AJK -> m IndexPage
firstPost (Ctx actionUrl _) = return . IndexPage actionUrl

firstPostSisanen :: MonadIO m => Ctx -> LomakeResult Sisanen -> m SisanenPage
firstPostSisanen (Ctx actionUrl _) = return . SisanenPage actionUrl

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
    body_ b

-------------------------------------------------------------------------------
-- WAI boilerplate
-------------------------------------------------------------------------------

server :: Ctx -> Server AJKLomakeAPI
server ctx@(Ctx actionUrl _) = pure (IndexPage actionUrl (LomakeResult emptyLomakeEnv Nothing))
    :<|> firstPost ctx
    :<|> secondPost ctx
    :<|> pure (SisanenPage actionUrl (LomakeResult emptyLomakeEnv Nothing))
    :<|> firstPostSisanen ctx

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
    let settings = Warp.defaultSettings
          & Warp.setPort port
          & Warp.setOnExceptionResponse onExceptionResponse
    Warp.runSettings settings (app ctx)

onExceptionResponse :: SomeException -> Response
onExceptionResponse exc =
    responseLBS status500 [] $ fromString $ "Exception: " ++  show exc
