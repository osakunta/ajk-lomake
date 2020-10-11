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
import Control.Lens              (forOf, ifor_)
import Control.Monad             (forM_, when)
import Control.Monad.IO.Class    (MonadIO (..))
import Data.FileEmbed            (embedStringFile)
import Data.Function             ((&))
import Data.List.NonEmpty        (NonEmpty)
import Data.Maybe                (fromMaybe)
import Data.Reflection           (give)
import Data.Semigroup            ((<>))
import Data.String               (fromString)
import Data.Text                 (Text)
import Futurice.Prelude
import Lucid                     hiding (for_)
import Network.HTTP.Types.Status (status500)
import Network.SendGridV3.Api    (MailAddress (..))
import Network.Wai
import Prelude ()
import Servant
import Servant.HTML.Lucid
import Servant.Multipart
import System.Environment        (lookupEnv)
import System.Exit               (ExitCode (..))
import System.IO                 (hPutStrLn, stderr, stdout)
import Text.Read                 (readMaybe)

import qualified Data.ByteString.Base64    as Base64
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.List.NonEmpty        as NE
import qualified Data.Text                 as T
import qualified Graphics.PDF              as PDF
import qualified Network.SendGridV3.Api    as SG
import qualified Network.Wai.Handler.Warp  as Warp
import qualified System.Process.ByteString as ProcBS

import Lomake
import Lomake.PDF

import SatO.AJK.Lomake.Asuntohaku
import SatO.AJK.Lomake.Classes
import SatO.AJK.Lomake.Huoltoilmoitus
import SatO.AJK.Lomake.Palaute
import SatO.AJK.Lomake.Sisainen

import qualified SatO.AJK.Lomake.Uusinta as U

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
    :<|> MultipartForm Mem (LomakeResult a) :> Post '[HTML] (Page a)
    :<|> "send" :> ReqBody '[FormUrlEncoded] (LomakeResult a) :> Post '[HTML] (ConfirmPage a)
    )

type AJKLomakeAPI =
    Get '[JSON] Text :<|>
    FormAPI Asuntohaku :<|>
    FormAPI Sisainen :<|>
    FormAPI Huoltoilmoitus :<|>
    FormAPI U.Uusinta :<|>
    FormAPI Palaute

instance (LomakeForm a, LomakeName a) => ToHtml (Page a) where
    toHtmlRaw _ = pure ()
    toHtml (Page (LomakeResult env v)) = page_ t $ do
        case v of
            Nothing -> do
                form_ [action_ $ "/" <> lomakeShortName p <> "/" , enctype_ "multipart/form-data", method_ "POST"] $ do
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
        div_ [class_ "row"] $ div_ [class_ "large-12 columns"] $ case sent of
            True  -> do
                div_ $ toHtml $ lomakeCompleted p
                when (lomakeRedo p) $
                    a_ [href_ $ "/" <> lomakeShortName p <> "/" ] $ "Täytä uusi"
            False -> div_ $ "Virhe! Jotain odottamatonta tapahtui. Kokeile hetken päästä uudestaan."
      where
        p = Proxy :: Proxy a
        t = lomakeTitle p


ajkLomakeApi :: Proxy AJKLomakeAPI
ajkLomakeApi = Proxy

firstPost :: (MonadIO m, LomakeForm a) => LomakeResult a -> m (Page a)
firstPost (LomakeResult env ma) = do
    env' <- forOf lomakePdfBS env $ \bs -> liftIO $ do
        (ec, bs', _) <- ProcBS.readProcessWithExitCode "gs"
            [ "-sDEVICE=pdfwrite"
            , "-dCompatibilityLevel=1.4"
            , "-dPDFSETTINGS=/screen"
            , "-dNOPAUSE"
            , "-dQUIET"
            , "-dBATCH"
            , "-sOutputFile=-"
            , "-"
            ]
            bs
        unless (ec == ExitSuccess) $ fail "PDF conversion failed"
        return bs'
    return $ Page $ LomakeResult env' ma

sendmail' :: SG.ApiKey -> SG.Mail () () -> IO ()
sendmail' apiKey mail = do
    print mail
    res <- SG.sendMail apiKey mail
    putStrLn $ "Result: " ++ show res

secondPost
    :: forall m a.
       (MonadIO m, LomakeForm a, LomakeEmail a, LomakeAddress a, LomakeName a)
    => MailAddress
    -> SG.ApiKey
    -> LomakeResult a
    -> m (ConfirmPage a)
secondPost _           _        (LomakeResult _ Nothing)    = pure $ ConfirmPage False
secondPost fromAddress sgApiKey (LomakeResult _ (Just ajk)) = do
    liftIO $ forM_ toAddresses $ \toAddress -> do
        -- empty mail
        let mail0 :: SG.Mail () ()
            mail0 = SG.mail [] fromAddress subject (SG.mailContentText body :| [])

        -- mail with attachments
        let mail1 = mail0
                { SG._mailAttachments = fmap toList . NE.nonEmpty $
                    -- supplied attachments
                    [ pdfAttachment n bs
                    | (n, bs) <- attachements pretty
                    ] ++
                    -- lomake data itself
                    [ pdfAttachment pdfName (LBS.toStrict pdfBS)
                    | lomakePdf proxyA
                    ]
        -- todo add attachments
                }

        -- mail to committee
        let mail = mail1
                { SG._mailPersonalizations = [SG.personalization $ toAddress :| []]
                , SG._mailReplyTo          = lomakeSend ajk
                }

        -- mail to applicant
        let mailCopy = lomakeSend ajk <&> \addr -> mail1
                { SG._mailPersonalizations = [SG.personalization $ addr :| []]
                , SG._mailReplyTo          = Just toAddress
                }

        hPutStrLn stderr $ "Sending application from " <> T.unpack name <> " to " <> show toAddress
        hPutStrLn stdout $ T.unpack body

        -- Send to reciepent
        sendmail' sgApiKey mail

        -- Send to applicant
        case mailCopy of
            Nothing -> return ()
            Just m -> sendmail' sgApiKey m

    pure $ ConfirmPage True
  where
    proxyA = Proxy :: Proxy a

    pdfAttachment :: Text -> ByteString -> SG.MailAttachment
    pdfAttachment n bs = SG.MailAttachment
        { SG._mailAttachmentContent     = decodeUtf8Lenient $ Base64.encode bs
        , SG._mailAttachmentType        = Just "application/pdf"
        , SG._mailAttachmentFilename    = n
        , SG._mailAttachmentDisposition = Nothing
        , SG._mailAttachmentContentId   = ""
        }

    pretty = lomakePretty ajk

    body :: Text
    body = render pretty

    pdfBS :: LBS.ByteString
    pdfBS = PDF.pdfByteString PDF.standardDocInfo a4rect pdf

    pdf :: PDF.PDF ()
    pdf = ifor_ (renderPDFText subject pretty) $ \n draw -> do
        page <- PDF.addPage Nothing
        PDF.drawWithPage page (renderPDFFooter n subject)
        PDF.drawWithPage page draw

    subject :: Text
    subject = lomakeEmailTitle proxyA <> " " <> name

    name :: Text
    name = lomakeSender ajk

    pdfName :: Text
    pdfName = T.replace " " "-" (T.takeWhile (/= '<') name) <> ".pdf"

    toAddresses :: NonEmpty MailAddress
    toAddresses = lomakeAddress proxyA

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
            div_ [class_ "large-1 medium-2 columns"] $ img_ [ src_ "https://asuntola.satakuntatalo.fi/images/talopiirros.jpg", style_ "width: 100px; margin: 5px;" ]
            div_ [class_ "large-11 medium-10 columns"] $ h1_ [ style_ "margin: 10px 0" ] $ toHtml t
        b

-------------------------------------------------------------------------------
-- WAI boilerplate
-------------------------------------------------------------------------------

formServer
    :: (LomakeForm a, LomakeEmail a, LomakeAddress a, LomakeName a)
    => MailAddress -> SG.ApiKey
    -> Server (FormAPI a)
formServer fromAddr sgApiKey =
         pure (Page $ LomakeResult emptyLomakeEnv Nothing)
    :<|> firstPost
    :<|> secondPost fromAddr sgApiKey

server
    :: NonEmpty MailAddress
    -> NonEmpty MailAddress
    -> MailAddress
    -> SG.ApiKey
    -> Server AJKLomakeAPI
server addr huoltoAddr fromAddr sgApiKey =
    give (SisainenAddress addr) $
    give (AsuntohakuAddress addr) $
    give (PalauteAddress addr) $
    give (HuoltoilmoitusAddress huoltoAddr) $
    give (U.UusintaAddress addr) $
    return "ping pong"
    :<|> formServer fromAddr sgApiKey
    :<|> formServer fromAddr sgApiKey
    :<|> formServer fromAddr sgApiKey
    :<|> formServer fromAddr sgApiKey
    :<|> formServer fromAddr sgApiKey

app
    :: NonEmpty MailAddress -- ^ AJK MailAddress
    -> NonEmpty MailAddress -- ^ Huolto MailAddress
    -> MailAddress
    -> SG.ApiKey
    -> Application
app addrAjk addrHuolto addrFrom sgApiKey = serve ajkLomakeApi (server addrAjk addrHuolto addrFrom sgApiKey)

lookupEnvWithDefault :: Read a => a -> String -> IO a
lookupEnvWithDefault def v = do
    x <- lookupEnv v
    return $ fromMaybe def (x >>= readMaybe)

defaultMain :: IO ()
defaultMain = do
    port <- lookupEnvWithDefault 8080 "PORT"
    ajkAddress     <- mkAddrs . fromMaybe "foo@example.com"                 <$> lookupEnv "LOMAKE_EMAILADDR"
    huoltoAddress  <- mkAddrs . fromMaybe "foo@example.com"                 <$> lookupEnv "LOMAKE_HUOLTO_EMAILADDR"
    fromAddress    <- mkAddr . T.pack . fromMaybe "noreply@example.com"     <$> lookupEnv "LOMAKE_FROM_EMAILADDR"
    sendgridApiKey <- SG.ApiKey . T.pack . fromMaybe "WRONGKEY" <$> lookupEnv "SENDGRID_API_KEY"
    hPutStrLn stderr "Hello, ajk-lomake-api is alive"
    hPutStrLn stderr "Starting web server"
    hPutStrLn stderr $ "http://localhost:" ++ show port
    let settings = Warp.defaultSettings
          & Warp.setPort port
          & Warp.setOnExceptionResponse onExceptionResponse
    Warp.runSettings settings $ app ajkAddress huoltoAddress fromAddress sendgridApiKey

onExceptionResponse :: SomeException -> Response
onExceptionResponse exc =
    responseLBS status500 [] $ fromString $ "Exception: " ++  show exc

mkAddr :: Text -> MailAddress
mkAddr addr = MailAddress addr ""

mkAddrs :: String -> NonEmpty MailAddress
mkAddrs t = mkAddr <$> case T.splitOn "," (T.pack t) of
    []     -> "" NE.:| []
    (x:xs) -> x NE.:| xs
