module Handler.GSN.Edit where

import JitCert.GSN
import Yesod.Form.Bootstrap3

import GSN
-- import GSN.GHC
import Import

data FormData = FormData {
      formDataSourceCode :: (String, (GSNGraph, [(Int, Text, GSNEnv)]))
    }

generateHtml :: Entity GSNE -> Maybe (Widget, Enctype) -> Handler Html
generateHtml (Entity gsnId gsnE) formM = defaultLayout $ do
    setTitle "Edit GSN"

    (widget, enctype) <- maybe (handlerToWidget $ generateFormPost $ renderForm $ gSNESource gsnE) return formM

    wrapErrorBlock

    [whamlet|
        <div .container>
            <div .row>
                <div .col-xs-12>
                    <h1>
                        Edit GSN
                    <form method=post enctype=#{enctype} role="form" action=@{GSNEditR gsnId} autocomplete="off">
                        ^{widget}
                        <div>
                            <a href="@{GSNR gsnId}" .btn .btn-default>
                                Cancel
                            <button type="submit" class="btn btn-primary">
                                Update
    |]



renderForm :: String -> Form FormData
renderForm defaultSource = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> areq gsnSourceField sourceFieldSettings (Just (defaultSource, error "unreachable"))

    where
        sourceFieldSettings' = bfs ("Source code" :: Text)
        sourceFieldSettings = sourceFieldSettings' {fsAttrs = ("rows","20"):fsAttrs sourceFieldSettings'}

postGSNEditR :: GSNEId -> Handler Html
postGSNEditR gsnId = do
    gsnE <- runDB $ get404 gsnId
    let gsnEE = Entity gsnId gsnE

    ((res,widget), enctype) <- runFormPost $ renderForm $ gSNESource gsnE
    case res of
        FormFailure _ -> 
            generateHtml gsnEE $ Just (widget, enctype)
        FormMissing -> 
            generateHtml gsnEE $ Just (widget, enctype)
        FormSuccess FormData{..} -> do
            let (source, gsn) = formDataSourceCode

            -- Update database.
            runDB $ update gsnId [GSNESource =. source]

            -- Update GSN in memory.
            updateGSN gsnId gsn

            -- Set message.
            setMessage [shamlet|Successfully updated GSN!|]

            -- Redirect.
            redirect $ GSNR gsnId

getGSNEditR :: GSNEId -> Handler Html
getGSNEditR gsnId = do
    gsnE <- runDB $ get404 gsnId

    generateHtml (Entity gsnId gsnE) Nothing



