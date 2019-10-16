module Handler.GSN.Refresh where

import Import

getGSNRefreshR :: Handler Html
getGSNRefreshR = do
    runDB $ deleteWhere ([] :: [Filter GSNE])

    redirect HomeR
