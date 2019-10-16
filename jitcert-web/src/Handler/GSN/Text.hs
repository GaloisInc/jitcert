module Handler.GSN.Text where

--import JitCert.GSN
import JitCert.Nlp2Dsl (nl2DslString)
import Yesod.Form.Bootstrap3

import qualified Data.Text as Text
import GSN.GHC
import GSN
import Import
import qualified Debug.Trace as DT

data FormData = FormData {
      formDataName :: Text
    , formDataSourceText :: Textarea
    }

renderForm :: Form FormData
renderForm = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> areq textField (bfs ("GSN Name" :: Text)) (Just "GSNfromNL")
    <*> areq textareaField (bfs ("Assurance case text" :: Text)) (Just defaultSource)

    where
        defaultSource = "The provided name of the cryptographic module (which will be on the validation certificate) shall be consistent with the defined cryptographic boundary as defined in the test report.  It is not acceptable to provide a module name that represents a module that has more components than the modules defined boundary.If it is desired to have a name that does represent a larger entity, then the cryptographic boundary must be consistent. All components residing within the cryptographic boundary must either be included (AS.01.08) or excluded (AS.01.09) in the test report."

generateHtml :: Maybe (Widget, Enctype) -> Handler Html
generateHtml formM = defaultLayout $ do
    setTitle "New GSN from Text"

    (widget, enctype) <- maybe (handlerToWidget $ generateFormPost renderForm) return formM

    wrapErrorBlock

    [whamlet|
        <div .container>
            <div .row>
                <div .col-xs-12>
                    <h1>
                        Create a new GSN from text
                    <form method=post enctype=#{enctype} role="form" action=@{GSNNewTextR} autocomplete="off">
                        ^{widget}
                        <button type="submit" class="btn btn-primary">
                            Create
    |]



getGSNNewTextR :: Handler Html
getGSNNewTextR = generateHtml Nothing

postGSNNewTextR :: Handler Html
postGSNNewTextR = do
    
    ((res,widget), enctype) <- runFormPost renderForm
    case res of
        FormFailure _ -> 
            generateHtml $ Just (widget, enctype)
        FormMissing -> 
            generateHtml $ Just (widget, enctype)
        FormSuccess FormData{..} -> do

            let text = Text.unpack $ unTextarea formDataSourceText
            source <- liftIO $ nl2DslString text

            res <- compileGSN source

            case res of
                Left err ->
                    DT.traceShow err (generateHtml $ Just (widget, enctype))
                Right gsn -> do

                    -- Insert into database.
                    gsnId <- runDB $ insert $ GSNE formDataName source

                    -- Update GSN in memory.
                    updateGSN gsnId gsn

                    -- Set message.
                    setMessage [shamlet|Successfully created GSN!|]

                    -- Redirect.
                    redirect $ GSNR gsnId


