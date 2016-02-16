module Handler.Simulate.MP where

import           Import
import           Data.Time           (addUTCTime)

import           Yesod.Helpers.Utils (randomHumanSafeStr)

import WeiXin.PublicPlatform


sendError :: MonadHandler m => Int -> Text -> m a
sendError ec msg = do
    sendResponse $ toJSON $
        WxppAppError
            (WxppErrorX $ Left ec)
            msg

getWxReqParam :: MonadHandler m
                => Int
                -> Text
                -> m Text
getWxReqParam ec name = do
    mv <- lookupGetParam name
    case mv of
        Nothing -> sendError ec $ "missing " <> name
        Just v  -> return v

-- | 取 access token
getSimulateTokenR :: Handler Value
getSimulateTokenR = do
    m_grant_type <- lookupGetParam "grant_type"
    when (m_grant_type /= Just "client_credential") $ do
        sendError 61451 "invalid client_credential"
    app_id <- WxppAppID <$> getWxReqParam 41002 "appid"
    secret <- WxppAppSecret <$> getWxReqParam 41004 "secret"
    m_rec <- runDB $ getBy $ UniqueMpApp app_id
    case m_rec of
        Nothing -> sendError 40013 "invalid appid"

        Just (Entity _rec_id rec) -> do
            let secret0 = mpAppSecret rec
            when (secret0 /= secret) $ do
                sendError 40001 "incorrect secret"

    -- make up access token
    now <- liftIO getCurrentTime
    let ttl    = 7200 :: Int
        expiry = addUTCTime (fromIntegral ttl) now
    atk <- randomHumanSafeStr 16
    runDB $ insert_ $ MadeAccessToken
                        app_id
                        atk
                        now
                        expiry
    return $ object
                [ "access_token" .= atk
                , "expires_in" .= ttl
                ]


-- | 建菜单
postSimulateMenuCreateR :: Handler Value
postSimulateMenuCreateR = do
    return $ toJSON $
        WxppAppError
            (WxppErrorX $ Left 0)
            ""
