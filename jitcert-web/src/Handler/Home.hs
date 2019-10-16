{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

-- import JitCert.GSN
-- import Text.Julius (RawJS (..))
-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import qualified Data.Text as T
import GSN
import Import

-- -- Define our data that will be used for creating the form.
-- data FileForm = FileForm
--     { fileInfo :: FileInfo
--     , fileDescription :: Text
--     }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = defaultLayout $ do

    setTitle "JitCert Web Viewer"

    -- Get GSNs.
    gsns <- handlerToWidget loadGSNs

    [whamlet|
        <div .container>
            <div .row>
                <div .col-xs-12>
                    <h1>
                        GSNs
                        <a href="@{GSNNewR}" .pull-right .btn .btn-sm .btn-primary style="margin: 3px 5px;">
                            New GSN
                        <a href="@{GSNNewTextR}" .pull-right .btn .btn-sm .btn-primary style="margin: 3px 5px;">
                            New GSN from text
                    <div>
                        <ul>
                            ^{concatMap gsnWidget gsns}
    |]

    where
        gsnWidget :: Entity GSNE -> Widget
        gsnWidget (Entity k e) = [whamlet|
            <li>
                <a href="@{GSNR k}">
                    #{gSNEName e}
        |]


        loadGSNs = do
            gsns <- runDB $ selectList [] [Asc GSNEId]

            if null gsns then do
                -- Insert GSNs.
                gs <- gsnList
                runDB $ mapM_ (\(n,g) -> insert_ $ GSNE n $ T.unpack g) gs

                -- Recursive call to load.
                loadGSNs
            else
                return (gsns :: [Entity GSNE])


