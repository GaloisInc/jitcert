module Handler.GSN.New where

import qualified Data.List as List
import qualified Data.Text as Text
import JitCert.GSN
import Yesod.Form.Bootstrap3

import GSN
import Import

data FormData = FormData {
      formDataName :: Text
    , formDataTemplate :: Int
    , formDataSourceCode :: (String, (GSNGraph, [(Int, Text, GSNEnv)]))
    }

renderForm :: Form FormData
renderForm = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> areq textField (bfs ("GSN Name" :: Text)) Nothing
    <*> areq (codeMirrorTemplateField gsnTemplates) (bfs ("Template" :: Text)) Nothing
    <*> areq gsnSourceField (bfs ("Source code" :: Text)) (Just (defaultSource, error "unreachable"))

    where
        defaultSource = Text.unpack $ snd $ List.head gsnTemplates


generateHtml :: Maybe (Widget, Enctype) -> Handler Html
generateHtml formM = defaultLayout $ do
    setTitle "New GSN"

    (widget, enctype) <- maybe (handlerToWidget $ generateFormPost renderForm) return formM

    wrapErrorBlock

    [whamlet|
        <div .container>
            <div .row>
                <div .col-xs-12>
                    <h1>
                        Create a new GSN
                    <form method=post enctype=#{enctype} role="form" action=@{GSNNewR} autocomplete="off">
                        ^{widget}
                        <button type="submit" class="btn btn-primary">
                            Create
    |]



getGSNNewR :: Handler Html
getGSNNewR = generateHtml Nothing

postGSNNewR :: Handler Html
postGSNNewR = do
    
    ((res,widget), enctype) <- runFormPost renderForm
    case res of
        FormFailure _ -> 
            generateHtml $ Just (widget, enctype)
        FormMissing -> 
            generateHtml $ Just (widget, enctype)
        FormSuccess FormData{..} -> do
            let (source, gsn) = formDataSourceCode

            -- Insert into database.
            gsnId <- runDB $ insert $ GSNE formDataName source

            -- Update GSN in memory.
            updateGSN gsnId gsn

            -- Set message.
            setMessage [shamlet|Successfully created GSN!|]

            -- Redirect.
            redirect $ GSNR gsnId

gsnTemplates :: [(Text,Text)]
gsnTemplates = [
      ("Basic", "module WebGSN where\n\
                \\n\
                \import JitCert.GSN.Builder\n\
                \import JitCert.GSN.Types\n\
                \\n\
                \\n\
                \data MyEvidence = MyEvidence\n\
                \data MySolutionNode = MySolutionNode\n\
                \\n\
                \instance SolutionType MySolutionNode where\n\
                \    type SolutionEvidence MySolutionNode = MyEvidence\n\
                \\n\
                \    renderSolution _s = [SFText \"My solution node\"]\n\
                \\n\
                \webGSN :: (GSNGraph, GSNEnv)\n\
                \webGSN = runBuilder $ do\n\
                \    g <- goal PolicyAnd [SFText \"Top level goal\"]\n\
                \\n\
                \    s <- solution MySolutionNode\n\
                \    addEdge g s\n\
                \\n\
                \    return ()\n\
                \ "
      )
    , ("JitCert Demo", "module WebGSN where\n\
                       \\n\
                       \import JitCert.GSN.Builder\n\
                       \import JitCert.GSN.Types\n\
                       \import qualified Data.Text                     as ST\n\
                       \import           Data.Text.Lazy                 ( Text )\n\
                       \\n\
                       \data MySolutionNode = MySolutionNode\n\
                       \data MyEvidence = MyEvidence\n\
                       \  \n\
                       \instance SolutionType MySolutionNode where\n\
                       \    type SolutionEvidence MySolutionNode = MyEvidence\n\
                       \    renderSolution _s = [SFText \"Presentation\"]\n\
                       \\n\
                       \webGSN :: (GSNGraph, GSNEnv)\n\
                       \webGSN = runBuilder $ do\n\
                       \    g <- goal PolicyOr [SFText \"The Demonstration will be successful\"]\n\
                       \    \n\
                       \    st1  <- strategy PolicyAnd [SFText \"Provide a presentation, some simple examples, and a real world example\"]\n\
                       \    addEdge g st1\n\
                       \    \n\
                       \    st2  <- strategy PolicyAnd [SFText \"Smile awkwardly\"]\n\
                       \    addEdge g st2\n\
                       \    \n\
                       \    sg1 <- goal PolicyAnd [SFText \"Provide Presentation\"]\n\
                       \    sg2 <- goal PolicyAnd [SFText \"Provide simple examples\"]\n\
                       \    sg3 <- goal PolicyAnd [SFText \"Demostrate relevant OpenSSL example\"]\n\
                       \\n\
                       \    addEdge st1 sg1\n\
                       \    addEdge st1 sg2\n\
                       \    addEdge st1 sg3\n\
                       \    \n\
                       \    s1 <- solution MySolutionNode\n\
                       \    addEdge sg1 s1\n\
                       \\n\
                       \    return ()\n\
                       \ "
      )
    , ("JitCert Demo with Context", "module WebGSN where\n\
                       \\n\
                       \import JitCert.GSN.Builder\n\
                       \import JitCert.GSN.Types\n\
                       \import qualified Data.Text                     as ST\n\
                       \import           Data.Text.Lazy                 ( Text )\n\
                       \\n\
                       \data MySolutionNode = MySolutionNode\n\
                       \data MyEvidence = MyEvidence\n\
                       \  \n\
                       \instance SolutionType MySolutionNode where\n\
                       \    type SolutionEvidence MySolutionNode = MyEvidence\n\
                       \    renderSolution _s = [SFText \"Presentation\"]\n\
                       \\n\
                       \webGSN :: (GSNGraph, GSNEnv)\n\
                       \webGSN = runBuilder $ do\n\
                       \    g <- goal PolicyOr [SFText \"The Demonstration will be successful\"]\n\
                       \    \n\
                       \    st1  <- strategy PolicyAnd [SFText \"Provide a presentation, some simple examples, and a real world example\"]\n\
                       \    addEdge g st1\n\
                       \    \n\
                       \    st2  <- strategy PolicyAnd [SFText \"Smile awkwardly\"]\n\
                       \    addEdge g st2\n\
                       \    \n\
                       \    sg1 <- goal PolicyAnd [SFText \"Provide Presentation\"]\n\
                       \    sg2 <- goal PolicyAnd [SFText \"Provide simple examples\"]\n\
                       \    sg3 <- goal PolicyAnd [SFText \"Demostrate relevant OpenSSL example\"]\n\
                       \\n\
                       \    addEdge st1 sg1\n\
                       \    addEdge st1 sg2\n\
                       \    addEdge st1 sg3\n\
                       \    \n\
                       \    s1 <- solution MySolutionNode\n\
                       \    addEdge sg1 s1\n\
                       \\n\
                       \    present <- contextVerb @PM \"is not present\" (\\_ -> False)\n\
                       \    addEdge st2 present\n\
                       \\n\
                       \    return ()\n\
                       \\n\
                       \\n\
                       \data PM = PM {\n\
                       \    name :: Text\n\
                       \  }\n\
                       \\n\
                       \instance RenderContext PM where\n\
                       \    renderContextTypeReference _ = \"PM\"\n\
                       \    renderContext (PM name) = name\n\
                       \\n\
                       \ "
      )
    , ("JitCert Demo with Example", "module WebGSN where\n\
                       \\n\
                       \import JitCert.Docs\n\
                       \import JitCert.GSN.Builder\n\
                       \import JitCert.GSN.Types\n\
                       \import qualified Data.Text                     as ST\n\
                       \import           Data.Text.Lazy                 ( Text )\n\
                       \\n\
                       \data MySolutionNode = MySolutionNode\n\
                       \data MyEvidence = MyEvidence\n\
                       \  \n\
                       \instance SolutionType MySolutionNode where\n\
                       \    type SolutionEvidence MySolutionNode = MyEvidence\n\
                       \    renderSolution _s = [SFText \"Presentation\"]\n\
                       \\n\
                       \webGSN :: (GSNGraph, GSNEnv)\n\
                       \webGSN = runBuilder $ do\n\
                       \    g <- goal PolicyOr [SFText \"The Demonstration will be successful\"]\n\
                       \    \n\
                       \    st1  <- strategy PolicyAnd [SFText \"Provide a presentation, some simple examples, and a real world example\"]\n\
                       \    addEdge g st1\n\
                       \    \n\
                       \    st2  <- strategy PolicyAnd [SFText \"Smile awkwardly\"]\n\
                       \    addEdge g st2\n\
                       \    \n\
                       \    sg1 <- goal PolicyAnd [SFText \"Provide Presentation\"]\n\
                       \    sg2 <- goal PolicyAnd [SFText \"Provide simple examples\"]\n\
                       \    sg3 <- goal PolicyAnd [SFText \"Demostrate relevant OpenSSL example\"]\n\
                       \\n\
                       \    addEdge st1 sg1\n\
                       \    addEdge st1 sg2\n\
                       \    addEdge st1 sg3\n\
                       \    \n\
                       \    s1 <- solution MySolutionNode\n\
                       \    addEdge sg1 s1\n\
                       \\n\
                       \    present <- contextVerb @PM \"is not present\" (\\_ -> False)\n\
                       \    addEdge st2 present\n\
                       \\n\
                       \    (_, _, example) <- figure7\n\
                       \    addEdge sg2 example\n\
                       \\n\
                       \    return ()\n\
                       \\n\
                       \\n\
                       \data PM = PM {\n\
                       \    name :: Text\n\
                       \  }\n\
                       \\n\
                       \instance RenderContext PM where\n\
                       \    renderContextTypeReference _ = \"PM\"\n\
                       \    renderContext (PM name) = name\n\
                       \\n\
                       \ "
      )
    ]

