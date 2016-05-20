{-# LANGUAGE ConstraintKinds     #-}
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
    Page(..),
    ) where

import Control.Exception         (SomeException)
import Control.Monad             (forM_)
import Control.Monad.IO.Class    (MonadIO (..))
import Data.FileEmbed            (embedStringFile)
import Data.Function             ((&))
import Data.List.NonEmpty        (NonEmpty)
import Data.Maybe                (fromMaybe)
import Data.Reflection           (give)
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

import qualified Data.List.NonEmpty       as NE
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import qualified Network.Wai.Handler.Warp as Warp

import Lomake

import SatO.AJK.Lomake.Asuntohaku
import SatO.AJK.Lomake.Classes
import SatO.AJK.Lomake.Huoltoilmoitus
import SatO.AJK.Lomake.Palaute
import SatO.AJK.Lomake.Sisainen

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

-- | TODO: the url
data Page a = Page
    { _pageResult :: !(LomakeResult a)
    }

newtype ConfirmPage a = ConfirmPage Bool -- Error

type FormAPI a = LomakeShortName a :>
    ( Get '[HTML] (Page a)
    :<|> ReqBody '[FormUrlEncoded] (LomakeResult a) :> Post '[HTML] (Page a)
    :<|> "send" :> ReqBody '[FormUrlEncoded] (LomakeResult a) :> Post '[HTML] (ConfirmPage a)
    )

type AJKLomakeAPI =
    FormAPI Asuntohaku :<|>
    FormAPI Sisainen :<|>
    FormAPI Huoltoilmoitus :<|>
    FormAPI Palaute

instance (LomakeForm a, LomakeName a) => ToHtml (Page a) where
    toHtmlRaw _ = pure ()
    toHtml (Page (LomakeResult env v)) = page_ t$ do
        case v of
            Nothing -> do
                form_ [action_ $ "/" <> lomakeShortName p <> "/" , method_ "POST"] $ do
                    maybe (pure ()) (div_ [class_ "row"] . div_ [class_ "large-12 columns secondary callout"] . span_ . toHtml) (lomakePreamble p)
                    lomakeView p env
                    div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
                        input_ [class_ "medium button", type_ "submit", value_ "Esikatsele"]
            Just ajk -> do
                div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
                    h2_ $ "Tarkista tietosi vielä kerran:"
                div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
                    pre_ $ toHtml $ render $ lomakePretty ajk
                hr_ []
                form_ [action_ $ "/" <> lomakeShortName p <> "/send", method_ "POST"] $ do
                    hiddenForm env
                    div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ do
                        input_ [class_ "medium button", type_ "submit", value_ "Lähetä"]
      where
        p = Proxy :: Proxy a
        t = lomakeTitle p

instance LomakeName a => ToHtml (ConfirmPage a) where
    toHtmlRaw _ = pure ()
    toHtml (ConfirmPage sent) = page_ t $
        div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $
            case sent of
                True  -> div_ $ toHtml $ lomakeCompleted p
                False -> div_ $ "Virhe! Jotain odottamatonta tapahtui. Kokeile hetken päästä uudestaan."
      where
        p = Proxy :: Proxy a
        t = lomakeTitle p


ajkLomakeApi :: Proxy AJKLomakeAPI
ajkLomakeApi = Proxy

firstPost :: MonadIO m => LomakeResult a -> m (Page a)
firstPost = return . Page

secondPost
    :: forall m a.
       (MonadIO m, LomakeForm a, LomakeEmail a, LomakeAddress a, LomakeName a)
    => LomakeResult a -> m (ConfirmPage a)
secondPost (LomakeResult _ Nothing) = pure $ ConfirmPage False
secondPost (LomakeResult _ (Just ajk)) = do
    liftIO $ forM_ toAddresses $ \toAddress -> do
        let mail = simpleMail' toAddress fromAddress subject body :: Mail
        hPutStrLn stderr $ "Sending application from " <> T.unpack name <> " to " <> show toAddress
        hPutStrLn stdout $ TL.unpack body
        bs <- renderMail' mail
        sendmail bs
    pure $ ConfirmPage True
  where
    body :: TL.Text
    body = TL.fromStrict $ T.pack $ render $ lomakePretty ajk

    subject :: Text
    subject = lomakeEmailTitle (Proxy :: Proxy a) <> " " <> name

    name :: Text
    name = lomakeSender ajk

    toAddresses :: NonEmpty Address
    toAddresses = lomakeAddress (Proxy :: Proxy a)

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
        div_ [class_ "row"] $ do
            div_ [class_ "large-1 medium-2 columns"] $ img_ [ src_ "http://asuntola.satakuntatalo.fi/images/talopiirros.jpg", style_ "width: 100px; margin: 5px;" ]
            div_ [class_ "large-11 medium-10 columns"] $ h1_ [ style_ "margin: 10px 0" ] $ toHtml t
        b

-------------------------------------------------------------------------------
-- WAI boilerplate
-------------------------------------------------------------------------------

formServer
    :: (LomakeForm a, LomakeEmail a, LomakeAddress a, LomakeName a)
    => Server (FormAPI a)
formServer =
         pure (Page $ LomakeResult emptyLomakeEnv Nothing)
    :<|> firstPost
    :<|> secondPost

server :: NonEmpty Address -> NonEmpty Address -> Server AJKLomakeAPI
server addr huoltoAddr =
    give (SisainenAddress addr) $
    give (AsuntohakuAddress addr) $
    give (PalauteAddress addr) $
    give (HuoltoilmoitusAddress huoltoAddr) $
    formServer :<|> formServer :<|> formServer :<|> formServer

app
    :: NonEmpty Address -- ^ AJK Address
    -> NonEmpty Address -- ^ Huolto Address
    -> Application
app addr addrHuolto = serve ajkLomakeApi (server addr addrHuolto)

lookupEnvWithDefault :: Read a => a -> String -> IO a
lookupEnvWithDefault def v = do
    x <- lookupEnv v
    return $ fromMaybe def (x >>= readMaybe)

defaultMain :: IO ()
defaultMain = do
    port <- lookupEnvWithDefault 8080 "PORT"
    emailAddr <- fromMaybe "foo@example.com" <$> lookupEnv "LOMAKE_EMAILADDR"
    huoltoAddr <- fromMaybe "foo@example.com" <$> lookupEnv "LOMAKE_HUOLTO_EMAILADDR"
    let ajkAddress = mkAddr emailAddr
    let huoltoAddress = mkAddr huoltoAddr
    hPutStrLn stderr "Hello, ajk-lomake-api is alive"
    hPutStrLn stderr "Starting web server"
    let settings = Warp.defaultSettings
          & Warp.setPort port
          & Warp.setOnExceptionResponse onExceptionResponse
    Warp.runSettings settings $ app ajkAddress huoltoAddress

onExceptionResponse :: SomeException -> Response
onExceptionResponse exc =
    responseLBS status500 [] $ fromString $ "Exception: " ++  show exc

mkAddr :: String -> NonEmpty Address
mkAddr t = Address Nothing <$> case T.splitOn "," (T.pack t) of
    []     -> "" NE.:| []
    (x:xs) -> x NE.:| xs
