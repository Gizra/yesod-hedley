module Handler.LoginToken where

import Import

getLoginTokenR :: Handler Value
getLoginTokenR = do
    -- Add token
    currentTime <- liftIO getCurrentTime
    tokenId <- insert $ Token "someToken" currentTime uid

    return $ object []
