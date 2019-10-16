module Handler.GSN where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as Set
import qualified Data.Text as Text
import JitCert.Analysis
-- import JitCert.DotBackend (evalToHighlight)
import JitCert.GSN
import JitCert.GSNAnalysis (EvaluationMap, evaluateArgument, evaluationMapToErrors)
import JitCert.Internal
import JitCert.Query
import Yesod.Form.Bootstrap3

import GSN
import Import hiding (undefined)

data FormData = FormData {
      formEnv :: Maybe Int
    , formCheckUnbound :: Bool
    , formCheckUnboundValues :: Bool
    , formCheckShadowedValues :: Bool
    , formCheckMultipleParents :: Bool
    , formNormalizeGSN :: Bool
    , formEvaluateGSN :: Bool
    , formHighlightDependencies :: Bool
    , formFindDirty :: Bool
    , formDirtyEnv :: Maybe Int
    }
$(Aeson.deriveJSON Aeson.defaultOptions ''FormData)

defaultFormData :: FormData
defaultFormData = FormData e True True False False False False False False e
    where
        e = Nothing

renderForm :: [(Int, Text, GSNEnv)] -> FormData -> Form FormData
renderForm envs' FormData{..} = renderBootstrap3 BootstrapBasicForm $ FormData
    <$> aopt selectFieldEnvs (bfs ("Environment" :: Text)) (Just formEnv)
    <*> areq (bootstrapCheckBoxField "Check for unbound errors.") "Unbound errors" (Just formCheckUnbound)
    <*> areq (bootstrapCheckBoxField "Check for values assigned to unbound node errors.") "Unbound value errors" (Just formCheckUnboundValues)
    <*> areq (bootstrapCheckBoxField "Check for shadowed values.") "Shadowed values" (Just formCheckShadowedValues)
    <*> areq (bootstrapCheckBoxField "Check for solution nodes that have multiple parents.") "Multiple parents" (Just formCheckMultipleParents)
    <*> areq (bootstrapCheckBoxField "Normalize GSN.") "Normalize" (Just formNormalizeGSN)
    <*> areq (bootstrapCheckBoxField "Logically evaluate GSN nodes") "Evaluate GSN" (Just formEvaluateGSN)
    <*> areq (bootstrapCheckBoxField "Highlight selected node's dependencies.") "Highlight dependencies" (Just formHighlightDependencies)
    <*> areq (bootstrapCheckBoxField "Find dirty nodes.") (addClassFieldSettings checkboxFieldDirtyEnvClass "Dirty with environment") (Just formFindDirty)
    <*> aopt selectFieldEnvs selectFieldEnvsSettings (Just formDirtyEnv)
    
    where
        selectFieldEnvs = selectFieldList envs

        envs = map (\(i, n, _) -> (n, i)) envs'
        -- envs = ("-", Nothing) : map (\(i, n, _) -> (n, Just i)) envs'

        selectFieldEnvsSettings = addClassFieldSettings selectFieldDirtyEnvClass $ ((if formFindDirty then id else withDisabled) $ bfs ("" :: Text))

checkboxFieldDirtyEnvClass :: Text
checkboxFieldDirtyEnvClass = "checkbox-dirty-env"

selectFieldDirtyEnvClass :: Text
selectFieldDirtyEnvClass = "select-dirty-env"

postGSNR :: GSNEId -> Handler Aeson.Value
postGSNR gsnId = do
    -- Get GSN from DB.
    gsnE <- runDB $ get404 gsnId

    -- Load GSNGraph and envs.
    (gsnGraph', gsnEnvs) <- lookupOrRedirectGSN gsnId gsnE

    -- Parse form.
    ((result, _widget), _enctype) <- runFormPost $ renderForm gsnEnvs defaultFormData -- Default form data shouldn't matter since we're in the post request.
    formData <- case result of
        FormSuccess f -> do
            saveFormData gsnId f
            return f
        FormMissing ->
            invalidArgs ["Invalid arguments"]
        FormFailure m -> do
            mapM_ $(logError) m
            invalidArgs ["Invalid arguments"]

    -- Process GSN with form data.
    (gsnGraph, gsnEnv, errs, dependenciesM, gsnHighlight, evalMapM) <- processGSN gsnGraph' gsnEnvs formData

    return $ Aeson.object [
        "graph" .= gsnToJSON gsnGraph gsnEnv (Just gsnHighlight)
      , "details" .= gsnToDetails gsnGraph gsnEnv evalMapM
      , "errors" .= map gsnErrorToMessage errs
      , "dependencies" .= gsnDependenciesToJSON gsnGraph dependenciesM
      , "highlighting" .= highlightMapToJSON gsnGraph (Just gsnHighlight) dependenciesM
      ]


runDiff :: GSNGraph -> [(Int, Text, GSNEnv)] -> GSNEnv -> FormData -> Handler (Set GSNError)
runDiff _gsnGraph _gsnEnvs _gsnEnv FormData{..} | formEnv == formDirtyEnv || not formFindDirty = 
    return mempty
runDiff gsnGraph gsnEnvs gsnEnv formData = do
    
    gsnDirtyEnv <- getGSNEnv gsnEnvs $ formDirtyEnv formData

    return $ Set.map WarningDirty $ envDiff gsnGraph gsnEnv gsnDirtyEnv

loadFormData :: GSNEId -> Handler FormData
loadFormData gsnId = do
    r <- lookupSessionBS $ formDataKey gsnId
    case r >>= Aeson.decodeStrict' of
        Nothing ->
            return defaultFormData
        Just f ->
            return f

saveFormData :: GSNEId -> FormData -> Handler ()
saveFormData k = setSessionBS (formDataKey k) . BSL.toStrict . Aeson.encode
    
formDataKey :: GSNEId -> Text
formDataKey k = "_formDataKey_" <> Text.pack (show k)

buildDependencies :: GSNGraph -> FormData -> Maybe DependencyGraph
buildDependencies _gsnGraph FormData{..} | not formHighlightDependencies = Nothing
buildDependencies gsnGraph _formData = Just $ buildReferencedDependencyGraph gsnGraph
    
runArgumentEvaluation :: GSNGraph -> GSNEnv -> FormData -> Maybe EvaluationMap
runArgumentEvaluation _ _ FormData{..} | not formEvaluateGSN = Nothing
runArgumentEvaluation gsnGraph gsnEnv _ = Just $ evaluateArgument gsnGraph gsnEnv

runQueries :: GSNGraph -> GSNEnv -> FormData -> Set GSNError
runQueries gsnGraph gsnEnv FormData{..} = mconcat [
    if formCheckUnbound then checkUnbound gsnGraph else mempty
  , if formCheckShadowedValues then checkShadowedValues gsnGraph gsnEnv else mempty
  , if formCheckUnboundValues then checkUnboundValues gsnGraph gsnEnv else mempty
  , if formCheckMultipleParents then checkSolutionMultipleParents gsnGraph else mempty
  ]

getGSNEnv :: Eq i => [(i, Text, GSNEnv)] -> Maybe i -> Handler GSNEnv
getGSNEnv gsnEnvs eIdM = case eIdM of
    Nothing ->
        return $ GSNEnv mempty mempty
    Just eId -> 
        case filter (\(eId', _, _) -> eId' == eId) gsnEnvs of
            [(_,_,e)] -> return e
            _ -> invalidArgs ["Invalid arguments"]

lookupOrRedirectGSN :: GSNEId -> GSNE -> Handler (GSNGraph, [(Int, Text, GSNEnv)])
lookupOrRedirectGSN gsnId gsnE = do
    res <- lookupGSN gsnId gsnE
    case res of
        Nothing -> do
            -- Set message.
            setMessage [shamlet|Invalid GSN. Please fix it.|]

            -- Redirect.
            redirect $ GSNEditR gsnId

        Just g -> return g

getGSNR :: GSNEId -> Handler Html
getGSNR gsnId = defaultLayout $ do

    addScript $ StaticR js_gsn_js
    addScript $ StaticR js_vis_network_min_js
    addStylesheet $ StaticR css_vis_network_min_css
    addStylesheet $ StaticR css_gsn_css

    -- TODO:
    -- Authenticate.
    -- Lookup GSN from database.
    -- Deserialize GSN.
    -- Initialize vis.js with GSN data.

    -- Get GSN from DB.
    gsnE <- handlerToWidget $ runDB $ get404 gsnId
    
    setTitle $ toHtml $ gSNEName gsnE

    -- Get GSNGraph.
    (gsnGraph', gsnEnvs) <- handlerToWidget $ lookupOrRedirectGSN gsnId gsnE

    -- Load form data.
    formData <- handlerToWidget $ loadFormData gsnId

    -- Process GSN.
    (gsnGraph, gsnEnv, errs, dependenciesM, gsnHighlight, evalMapM) <- handlerToWidget $ processGSN gsnGraph' gsnEnvs formData

    -- Build GSN.
    buildGSN gsnGraph gsnEnv (Just gsnHighlight) dependenciesM evalMapM
    
    -- Setup callback.
    toWidget [julius|
        (function() {
            $('#resubmit').submit( function( e) {
                e.preventDefault();

                var errorHandler = function( xhr) {
                    console.log( xhr.responseText);
                    alert( "Error updating GSN.");
                };

                var handler = function( data) {
                    // Update GSN with data.
                    gsn.build( "gsngraph", "gsndetails", data['graph'], data['details'], data['dependencies'], data['highlighting']);

                    var errs = data['errors'];

                    var errsDiv = $("#errors");

                    if ( errs.length === 0) {
                        errsDiv.html( "No errors or warnings.");
                    }
                    else {
                        errsDiv.html( "");
                        for (i in errs) {
                            var t = $("<span style='color:red'/>");
                            t.text( errs[i]);
                            var row = $("<div>");
                            row.append( t);
                            errsDiv.append(row);
                        }
                    }

                    // Update details.
                    $('#gsndetails').html(#{noNodeSelectedHtml});

                };

                $.ajax({
                    type: 'POST'
                  , url: $(this).attr('action')
                  , data: $(this).serialize()
                  , success: handler
                  , error: errorHandler
                });
            });

            $("."+#{checkboxFieldDirtyEnvClass}).change(function() {
                var select = $("."+#{selectFieldDirtyEnvClass});
                if ( this.checked) {
                    select.prop( "disabled", false);
                } else {
                    select.prop( "disabled", true);
                }
            });
        })();
    |]

    -- Generate and render Yesod form.
    ( widget, enctype) <- handlerToWidget $ generateFormPost $ renderForm gsnEnvs formData

    [whamlet|
        <div .container>
            <div .row>
                <div .col-xs-12>
                    <h1>
                        #{gSNEName gsnE}
                        <a href="@{GSNEditR gsnId}" .pull-right .btn .btn-sm .btn-default style="margin: 3px 0px;">
                            Edit
            <div .row style="display:none;">
                <div .col-md-12>
                    <div id="buttons">
                        <div .btn-group>
                            <button type="button" class="btn btn-default" id="btn-node-add">
                                <span .glyphicon-plus .glyphicon>
                            <button type="button" class="btn btn-default" id="btn-node-remove">
                                <span .glyphicon-minus .glyphicon>
            <div .row>
                <div .col-md-9>
                    <div id="gsngraph">
                <div .col-md-3>
                    <div id="sidebar-tabs">
                        <ul .nav-tabs .nav>
                            <li .active>
                                <a href="#options" data-toggle="tab">
                                    Options
                            <li>
                                <a href="#gsndetails" data-toggle="tab">
                                    Details
                                
                        <div .tab-content>
                            <div .tab-pane .active id="options">
                                <form id="resubmit" enctype=#{enctype} role="form" action=@{GSNR gsnId} autocomplete="off">
                                    ^{widget}
                                    <button type="submit" class="btn btn-default">
                                        Update
                            <div .tab-pane id="gsndetails">
                                #{noNodeSelectedHtml}
            <div .row>
                <div .col-md-12>
                    <div>
                        <h2>
                            Errors
                        <div id="errors">
                            ^{errorsW errs}
    |]

    where
        errorsW [] = [shamlet|No errors or warnings.|]
        errorsW es = concatMap (\e -> [shamlet|
            <div>
                <span style="color:red"/>
                    #{gsnErrorToMessage e}
          |]) es

processGSN :: GSNGraph -> [(Int, Text, GSNEnv)] -> FormData -> Handler (GSNGraph, GSNEnv, [GSNError], Maybe DependencyGraph, GSNHighlight, Maybe EvaluationMap)
processGSN gsnGraph' gsnEnvs formData = do
    -- Normalize GSN.
    let gsnGraph = if formNormalizeGSN formData then
            normalizeGSN gsnGraph'
          else
            gsnGraph'

    -- Extract env.
    gsnEnv <- getGSNEnv gsnEnvs $ formEnv formData

    -- Run queries.
    let rs = runQueries gsnGraph gsnEnv formData

    -- Evaluate GSN. 
    let evalMapM = runArgumentEvaluation gsnGraph gsnEnv formData
    let evalHighlight = mempty -- maybe mempty evalToHighlight evalMapM -- The blue is hard to read, so uncommenting for now.
    let evalEs = maybe mempty evaluationMapToErrors evalMapM

    -- Run diff.
    ds <- runDiff gsnGraph gsnEnvs gsnEnv formData 
    let errs = Set.toList $ ds <> rs <> evalEs

    -- Build dependency graph.
    let dependenciesM = buildDependencies gsnGraph formData

    -- Build GSNHighlight.
    let gsnHighlight = gsnErrorsToHighlight errs
    let highlight = gsnHighlight <> evalHighlight

    return (gsnGraph, gsnEnv, errs, dependenciesM, highlight, evalMapM)


noNodeSelectedHtml :: Html
noNodeSelectedHtml = [shamlet|
        <p>
            No node selected.
    |]

