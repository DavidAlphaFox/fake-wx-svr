module Handler.Admin.MP where

import Import
import qualified Data.Text as T

import Yesod.Helpers.Form
import Yesod.Helpers.Form2
import Yesod.Helpers.Handler

import WeiXin.PublicPlatform


getMpAppListR :: Handler Html
getMpAppListR = do
    app_list <- runDB $ selectList [ MpAppDeleted ==. False ] [ Desc MpAppId ]
    defaultLayout $ do
        setTitle "公众号列表"
        $(widgetFile "admin/app_list")

getMpAppNewR :: Handler Html
getMpAppNewR = do
    generateEMFormPost (appForm Nothing)
        >>= runReaderT (showAppForm MpAppNewR Nothing)

postMpAppNewR :: Handler Html
postMpAppNewR = do
    (((result, formWidget), formEnctype), form_errs) <- runEMFormPostNoToken (appForm Nothing)

    let showf merr = do
            m_add_err <- liftM (fromMaybe mempty) $ forM merr $ \err -> do
                            return $ overallFieldError err
            flip runReaderT (((formWidget, formEnctype), mempty), form_errs <> m_add_err) $ do
                showAppForm MpAppNewR merr

    case result of
        FormMissing     -> showf Nothing
        FormFailure _   -> showf Nothing

        FormSuccess rec -> do
            runDB $ insert_ rec
            redirect MpAppListR

getMpAppEditR :: MpAppId -> Handler Html
getMpAppEditR rec_id = do
    rec <- runDB $ get404 rec_id
    generateEMFormPost (appForm (Just rec))
        >>= runReaderT (showAppForm (MpAppEditR rec_id) Nothing)

postMpAppEditR :: MpAppId -> Handler Html
postMpAppEditR rec_id = do
    rec <- runDB $ get404 rec_id
    (((result, formWidget), formEnctype), form_errs) <- runEMFormPostNoToken (appForm (Just rec))

    let showf merr = do
            m_add_err <- liftM (fromMaybe mempty) $ forM merr $ \err -> do
                            return $ overallFieldError err
            flip runReaderT (((formWidget, formEnctype), mempty), form_errs <> m_add_err) $ do
                showAppForm (MpAppEditR rec_id) merr

    case result of
        FormMissing     -> showf Nothing
        FormFailure _   -> showf Nothing

        FormSuccess new_rec -> do
            runDB $ replace rec_id new_rec
            redirect MpAppListR

appForm :: Maybe MpApp -> MkEMForm App IO MpApp
appForm m_old extra = renderBootstrapES' extra $ do
    app_id <- semreq
                (convertField WxppAppID unWxppAppID strippedTextField)
                (labelNameToFs (asText "App ID") "app_id")
                (mpAppAppId <$> m_old)
    secret <- semreq
                (convertField WxppAppSecret unWxppAppSecret strippedTextField)
                (labelNameToFs (asText "Secret") "secret")
                (mpAppSecret <$> m_old)
    token <- semreq
                (convertField Token unToken strippedTextField)
                (labelNameToFs (asText "Token") "token")
                (mpAppToken <$> m_old)
    aes_key <- semreq aesKeyField
                (labelNameToFs (asText "AES Key") "aes_key")
                (mpAppAesKey <$> m_old)
    deleted <- case m_old of
                Nothing -> return $ pure False
                Just old -> semreq boolField
                                (labelNameToFs (asText "Delete?") "deleted")
                                (Just $ mpAppDeleted old)
    created_time <- case m_old of
                Nothing -> pure <$> liftIO getCurrentTime
                Just old -> return $ pure $ mpAppCreatedTime old
    return $ MpApp <$> app_id
                    <*> secret
                    <*> token
                    <*> aes_key
                    <*> deleted
                    <*> created_time


showAppForm :: Route App
            -> Maybe Text
            -> EFormHandlerT App IO Html
showAppForm action m_err_msg = do
    (((formWidget, formEnctype), _extra), form_errs) <- ask
    lift $ defaultLayout $ do
        setTitle "注册公众号"
        [whamlet|
          $maybe err_msg <- m_err_msg
             <div .form-group>
               <span .err_msg>_{err_msg}
          <ul>
            $forall ((_name, fs), errs) <- fieldErrorsToList form_errs
              <li>_{fsLabel fs}: #{T.intercalate ";" errs}
          <form method=post action="@{action}" enctype=#{formEnctype}>
            ^{formWidget}
          <input type=submit value=Submit>
        |]
