module GSN (buildGSN, updateGSN, lookupGSN, gsnToJSON, gsnToDetails, gsnList, gsnDependenciesToJSON, highlightMapToJSON) where

import qualified Data.Aeson as Aeson
import qualified Data.Graph.Inductive as FGL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import qualified Data.Text as TS
import qualified Text.Blaze.Internal as Html
import qualified Text.Blaze.Html5 as Html

import JitCert.DocGenerator (dropTypes)
import JitCert.DocGenerator.Types
import JitCert.Docs
import JitCert.DotBackend
import JitCert.GSN
import JitCert.GSNAnalysis
import JitCert.Internal
import JitCert.Query
import Type.Reflection

import GSN.GHC
import Import

updateGSN :: GSNEId -> (GSNGraph, [(Int, Text, GSNEnv)]) -> Handler ()
updateGSN gId g = do
    mv <- appGSNMap <$> getYesod
    modifyMVar_ mv $ return . Map.insert gId g
        
gsnList :: MonadIO m => m [(Text, Text)]
gsnList = do
    es <- embeddedGSNs
    ss <- stringGSNs
    return $ es <> ss

    where
        embeddedGSNs = return $ map (\(n, r, b) -> 
                -- Hacks to fix module name.
                -- TODO: Can we improve this??
                let (preM, postM) = TS.breakOn "module" r in
                let (_, postW) = TS.breakOn "where" postM in
                let r' = preM <> "module WebGSN " <> postW in

                (n, r' <> b)
            ) [
              ("Functions are correct", functionsAreCorrectRaw, "\n\nwebGSN :: (GSNGraph,GSNEnv)\nwebGSN = runBuilder (figure5 >> return ())")
            , ("Invalid GSN reference", unboundErrorRaw, "\n\nwebGSN :: (GSNGraph,GSNEnv)\nwebGSN = runBuilder (figureUnboundError >> return ())")
            , ("Program is safe (simplified)", programIsSafeSimplifiedRaw, "\n\nwebGSN :: (GSNGraph,GSNEnv)\nwebGSN = runBuilder (figure8 >> return ())")
            , ("Program is safe", programIsSafeRaw, "\n\nwebGSN :: (GSNGraph,[GSNEnv])\nwebGSN = \n\
                \    let (gsnGraph, e1) = runBuilder $ do\n\
                \          (_, rng, _) <- figure7\n\
                \          setContextValue rng $ RNG \"Dual_EC_DRBG\"\n\
                \    in\n\
                \    let (_, e2) = runBuilder $ do\n\
                \          (_, rng, _) <- figure7\n\
                \          setContextValue rng $ RNG \"HMAC_DRBG\"\n\
                \    in\n\
                \\n\
                \    (gsnGraph, [e1, e2])\n"
                )
            , ("OpenSSL Boundary", opensslRaw, "\n\nwebGSN :: (GSNGraph, [GSNEnv])\nwebGSN = runOpenSSLEnvs")
            , ("OpenSSL Certificate Caveats", opensslRaw, "\nwebGSN :: (GSNGraph, GSNEnv)\nwebGSN = runBuilder openSSLCertCaveat")
            , ("UTM", utmRaw, "\n\nwebGSN :: (GSNGraph, GSNEnv)\nwebGSN = runBuilder utm")
            ]

        stringGSNs = mapM (\(a,b) -> (a,) <$> checkGSN b) [
              ("Figure 3", "import JitCert.Docs\nimport JitCert.GSN\n\n\nwebGSN :: (GSNGraph, [GSNEnv])\nwebGSN = (:[]) <$> runBuilder (figure3 >> return ())")
            -- , ("Figure 7", "import JitCert.Docs\nimport JitCert.GSN\n\n\nwebGSN :: (GSNGraph, [GSNEnv])\nwebGSN = (:[]) <$> runBuilder (figure5 >> return ())")
            -- , ("Figure 8", "import JitCert.Docs\nimport JitCert.GSN\n\n\nwebGSN :: (GSNGraph, [GSNEnv])\nwebGSN = (:[]) <$> runBuilder figureUnboundError")
            , ("Figure 9", "import JitCert.Docs\nimport JitCert.Docs.Shared\nimport JitCert.GSN\n\n\nwebGSN :: (GSNGraph, [GSNEnv])\nwebGSN = fmap (:[]) $ runBuilder $ do\n\
                  \    (fs, rng, _) <- figure7\n\
                  \\n\
                  \    let functions = [ (Function \"fibonacci\" \"c9a0ad2c\", RNG \"rng\")\n\
                  \                , (Function \"foo\" \"f9434869\", RNG \"rng\")]\n\
                  \    setContextValuesWith fs functions $ \\(function, generator) -> do \n\
                  \\n\
                  \        setContextValue rng generator\n\
                  \\n\
                  \        return function\n\
                  \ ")
            -- , ("Figure 10", "import JitCert.Docs\nimport JitCert.GSN\n\n\nwebGSN :: (GSNGraph, [GSNEnv])\nwebGSN = (:[]) <$> runBuilder figure8")
            -- , ("Figure 11/12", "import JitCert.Docs\nimport JitCert.GSN\n\n\nwebGSN :: (GSNGraph, [GSNEnv])\nwebGSN = \n\
            --     \    let (gsnGraph, e1) = runBuilder figure9 in\n\
            --     \    let (_, e2) = runBuilder figure10 in\n\
            --     \\n\
            --     \    (gsnGraph, [e1, e2])\n"
            --     )
            , ("NOT Figure", "import JitCert.Docs\nimport JitCert.GSN\n\n\nwebGSN :: (GSNGraph, [GSNEnv])\nwebGSN = (:[]) <$> runBuilder figureNot")
            -- , ("OpenSSL Boundary", "import JitCert.Docs\nimport JitCert.GSN\nimport JitCert.Examples.OpenSSL\n\nwebGSN :: (GSNGraph, [GSNEnv])\nwebGSN = runOpenSSLEnvs")
            -- , ("OpenSSL Certificate Caveats", "import JitCert.Docs\nimport JitCert.GSN\nimport JitCert.Examples.OpenSSL\n\nwebGSN :: (GSNGraph, [GSNEnv])\nwebGSN = (:[]) <$> runBuilder openSSLCertCaveat")
            , ("Multiple parents", "import JitCert.GSN.Types\n\
                \\n\
                \\n\
                \data MySolutionNode = MySolutionNode\n\
                \\n\
                \instance SolutionType MySolutionNode where\n\
                \    type SolutionEvidence MySolutionNode = ()\n\
                \    validEvidence () = True\n\
                \\n\
                \    renderSolution _s = [SFText \"My solution node\"]\n\
                \\n\
                \webGSN :: (GSNGraph, GSNEnv)\n\
                \webGSN = runBuilder $ do\n\
                \    g <- goal PolicyAnd [SFText \"Top level goal\"]\n\
                \    \n\
                \    g1 <- goal PolicyAnd [SFText \"Goal 1\"]\n\
                \    addEdge g g1\n\
                \    \n\
                \    g2 <- goal PolicyAnd [SFText \"Goal 1\"]\n\
                \    addEdge g g2\n\
                \\n\
                \    s <- solution MySolutionNode\n\
                \    addEdge g1 s\n\
                \    addEdge g2 s\n\
                \\n\
                \    return ()"
                )
            -- , ("UTM", "import JitCert.Docs\nimport JitCert.GSN\nimport JitCert.Examples.UTM\n\nwebGSN :: (GSNGraph, [GSNEnv])\nwebGSN = (:[]) <$> runBuilder utm")
            ]

        checkGSN e = do
            let s = defaultSource e
            -- gsn <- compileGSN s
            -- case gsn of
            --     Left () -> error "gsnList doesn't compile"
            --     Right _ -> return s
            return s

        defaultSource e = "module WebGSN where\n\
                          \\n\
                          \import Control.Monad\n\
                          \\n\
                          \import JitCert.GSN.Builder\n\
                          \\n\
                          \\n" <> e



lookupGSN :: GSNEId -> GSNE -> Handler (Maybe (GSNGraph, [(Int, Text, GSNEnv)]))
lookupGSN gId gsnE = do
    -- Read MVar.
    app <- getYesod
    gsnM <- Map.lookup gId <$> readMVar (appGSNMap app)

    case gsnM of
        Just g ->
            return $ Just g

        Nothing -> do
            -- Compile GSN if it's not in the cache.
            gE <- compileGSN $ gSNESource gsnE

            case gE of
                Left _ ->
                    return Nothing
                Right g -> do

                    -- Set GSN.
                    updateGSN gId g

                    return $ Just g

gsnToJSON :: GSNGraph -> GSNEnv -> Maybe GSNHighlight -> Aeson.Value
gsnToJSON graph env highlightM = Aeson.object [
      "nodes" .= gsnNodesToJSON graph env highlightM
    , "edges" .= gsnEdgesToJSON graph
    ]

gsnNodesToJSON :: GSNGraph -> GSNEnv -> Maybe GSNHighlight -> Aeson.Value
gsnNodesToJSON GSNGraph{..} gsnEnv highlightM = Aeson.toJSON $ map (buildNode (Just gsnEnv) highlightM) $ FGL.labNodes gsnGraph

gsnEdgesToJSON :: GSNGraph -> Aeson.Value
gsnEdgesToJSON GSNGraph{..} = Aeson.toJSON $ map buildEdge $ FGL.labEdges gsnGraph
    where
        buildEdge (a, b, (InContextOf, _)) = Aeson.object [
              "from" .= a
            , "to" .= b
            , "color" .= Aeson.object [
                "color" .= ("#2B7CE9" :: Text)
              , "highlight" .= ("#2B7CE9" :: Text)
              ]
            , "arrows" .= Aeson.object [
                "to" .= Aeson.object [
                    "type" .= ("circle" :: Text) -- TODO: Figure out how to make this an open arrow.
                  ]
              ]
            ]
        buildEdge (a, b, (SupportedBy, _)) = Aeson.object [
              "from" .= a
            , "to" .= b
            , "color" .= Aeson.object [
                "color" .= ("#2B7CE9" :: Text)
              , "highlight" .= ("#2B7CE9" :: Text)
              ]
            ]


buildGSN :: GSNGraph -> GSNEnv -> Maybe GSNHighlight -> Maybe DependencyGraph -> Maybe EvaluationMap -> Widget
buildGSN gsnGraph gsnEnv highlightM depGraphM evalMapM = do
  toWidget [julius|
    (function() {
        gsn.build( "gsngraph", "gsndetails", #{gsnToJSON gsnGraph gsnEnv highlightM}, #{gsnToDetails gsnGraph gsnEnv evalMapM}, #{gsnDependenciesToJSON gsnGraph depGraphM}, #{highlightMapToJSON gsnGraph highlightM depGraphM});
    })();
  |]

gsnDependenciesToJSON :: GSNGraph -> Maybe DependencyGraph -> Aeson.Value
gsnDependenciesToJSON GSNGraph{..} depGraphM = case depGraphM of
    Nothing -> Aeson.Null
    Just dg -> 
        
        Aeson.object $ map (\(k,vs) -> 
            -- Filter out hidden nodes.
            let vs' = Set.toList $ vs Set.\\ hiddenSet in
            let k' = TS.pack $ show k in
            k' .= map (\v ->
                Aeson.object ["id" .= v, "color" .= dependencyColor]
              ) vs'
          ) $ Map.toList dg

    where
        dependencyColor :: Text
        dependencyColor = "#6BF971"

        hiddenSet = Set.fromList $ map (_sNodeId . snd) $ Map.toList gsnHiddenNodes

highlightMapToJSON :: GSNGraph -> Maybe GSNHighlight -> Maybe DependencyGraph -> Aeson.Value
highlightMapToJSON _ _ Nothing = Aeson.Null
highlightMapToJSON GSNGraph{..} highlightM _ = Aeson.toJSON $ map (\k -> 
      let color = case highlightM >>= (Map.lookup k . unGSNHighlight) of
            Nothing ->
                defaultColor
            Just v -> 
                Aeson.toJSON $ show v
      in
      Aeson.object ["id" .= k, "color" .= color]
    ) $ FGL.nodes gsnGraph

    where
        defaultColor = Aeson.object ["background" .= ("#97C2FC" :: Text), "border" .= ("#2B7CE9" :: Text)]

gsnToDetails :: GSNGraph -> GSNEnv -> Maybe EvaluationMap -> Aeson.Value
gsnToDetails g@GSNGraph{..} gsnEnv em = Aeson.object $ map toDetails $ FGL.labNodes gsnGraph
    where
        toDetails (nId, a) = 
            let tnId = TS.pack (show nId) in
            tnId .= [shamlet|
                <form>
                    #{labelHtml a}
                    #{policyHtml a}
                    #{valuesHtml a}
                    #{evaluationHtml a}
                    #{referenceHtml a}
                    #{docgenHtml nId g}
            |]

        
        labelHtml n = case lnodeToText (someNodeToLNodeWithEnv 0 gsnEnv n) of
            l | Text.null l -> 
                mempty
            l -> 
                [shamlet|
                    <div .form-group>
                        <label>
                            Label
                        <p .form-control-static>
                            #{l}
                |]

        evaluationHtml n = case Map.lookup n =<< em of
            Nothing -> mempty
            Just b ->
                [shamlet|
                <div .form-group>
                    <label>
                        Logical Evaluation
                    <p .form-control-static>
                        #{b}
                |]

        valuesHtml a = case valuesHtml' a of
          Nothing -> mempty
          Just m ->
            [shamlet|
                <div .form-group>
                    <label>
                        Values
                    <p .form-control-static>
                        #{m}
            |] 

        valuesHtml' :: SomeNode -> Maybe Text.Text
        valuesHtml' (SomeNode (Node ContextRepr nId _ _ _)) = Just $ Text.intercalate ", " $ map renderSomeContext $ lookupContextNodeValues nId g gsnEnv
        valuesHtml' (SomeNode n@(Node SolutionRepr nId (SolutionContent _) _ _)) = 
            let v = mapMaybe (\se -> case castEvidence (typeOf n) se of
                    Just e -> Just $ renderEvidence e
                    Nothing -> Nothing
                  ) $ lookupSolutionNodeValues nId gsnEnv
            in
            case v of
                [] -> Nothing
                _ -> Just $ Text.intercalate ", " v
        valuesHtml' _ = Nothing
        -- JP: Could show solutions too if we add a `renderEvidence`.

        castEvidence :: forall n . SolutionType n => TypeRep (Node n Solution) -> SomeEvidence -> Maybe (SolutionEvidence n)
        castEvidence _ (SomeEvidence e) = 
            let tN = typeRep :: TypeRep (SolutionEvidence n) in
            let tE = typeOf e in
            case eqTypeRep tN tE of
                Just HRefl -> Just e
                Nothing -> Nothing

        renderSomeContext (SomeContext c) = renderContext c


        referenceHtml (SomeNode (Node _ _ _ _ (Just ref))) = [shamlet|
            <div .form-group>
                <label>
                    Reference
                <p .form-control-static>
                    #{ref}

        |]
        referenceHtml _ = mempty
        
        policyHtml a = case policyHtml' a of
          Nothing -> mempty
          Just m ->
            [shamlet|
                <div .form-group>
                    <label>
                        Policy
                    <p .form-control-static>
                        #{m}
            |] 

        policyHtml' :: SomeNode -> Maybe Text
        policyHtml' (SomeNode (Node ContextRepr _ _ _ _)) = Nothing
        policyHtml' (SomeNode (Node SolutionRepr _ _ _ _)) = Nothing
        policyHtml' (SomeNode (Node AssumptionRepr _ _ _ _)) = Nothing
        policyHtml' (SomeNode (Node JustificationRepr _ _ _ _)) = Nothing
        policyHtml' (SomeNode (Node GoalRepr _ p _ _)) = Just $ policyToText $ nodePolicyContentToNodePolicy p
        policyHtml' (SomeNode (Node StrategyRepr _ p _ _)) = Just $ policyToText $ nodePolicyContentToNodePolicy p

        policyToText PolicyAnd = "AND"
        policyToText PolicyOr = "OR"
        policyToText (PolicyNot p) = "NOT . " <> policyToText p
        policyToText (PolicyForall _ _ p) = "FORALL . " <> policyToText p
        policyToText (PolicyWhen _ _ p) = "WHEN . " <> policyToText p

        docgenHtml nId g =
          let content = concatMap docToHtml (dropTypes (docGenNodeID g nId)) in
          if Html.null content then
            mempty
          else
            [shamlet|
              <div .form-group>
                <label>
                  Generated Documentation
                <div .form-control-static>
                  #{content}
            |]

        -- JP: You could also `Doc` a ToMarkup instance so you can use
        -- [toHtml](https://hackage.haskell.org/package/blaze-html-0.9.1.1/docs/Text-Blaze-Html.html#v:toHtml)
        docToHtml :: Doc -> Html
        docToHtml (Paragraph ds) = Html.p $ intercalate (Html.toHtml (" " :: Text.Text)) $ map docToHtml ds
        docToHtml (List ds) = Html.ul $ concatMap (Html.li . docToHtml) ds
        docToHtml (NumList ds) = Html.ol $ concatMap (Html.li . docToHtml) ds
        docToHtml (Heading ds) = Html.h2 $ intercalate (Html.toHtml (" " :: Text.Text)) $ map docToHtml ds
        docToHtml (Txt t) = Html.toHtml t


lnodeToText :: NLabel -> Text.Text
lnodeToText (Left Square) = "" -- "AND"
lnodeToText (Left Diamond) = "" -- "OR"
lnodeToText (Left Triangle) = "" -- "NOT"
lnodeToText (Right (_,label)) = label
    

buildNode :: Maybe GSNEnv -> Maybe GSNHighlight -> (FGL.Node, SomeNode) -> Aeson.Value
buildNode envM highlightM (_, n) = 
    let label = lnodeToText $ maybe (someNodeToLNode 0 n) ((flip (someNodeToLNodeWithEnv 0)) n) envM in
    -- let label = Text.pack $ show $ _sNodeId n in
    Aeson.object $ [
        "id" .= _sNodeId n
      , "label" .= label
      , "shape" .= nodeToShape n
      ] 
      <> nodeToRadius n
      <> nodeToColor n highlightM

    where
        nodeToRadius :: Aeson.KeyValue kv => SomeNode -> [kv]
        nodeToRadius (SomeNode (Node GoalRepr _ _ _ _)) = 
            ["shapeProperties" .= Aeson.object ["borderRadius" .= (0 :: Int)]]
        nodeToRadius _ = []

        nodeToColor :: Aeson.KeyValue kv => SomeNode -> Maybe GSNHighlight -> [kv]
        nodeToColor _ Nothing = []
        nodeToColor n (Just (GSNHighlight highlight)) = 
            let nid = _sNodeId n in
            case Map.lookup nid highlight of
                Nothing -> []
                Just c -> 
                    ["color" .= show c]

        nodeToShape :: SomeNode -> Text
        nodeToShape (SomeNode (Node ContextRepr _ _ _ _)) = "box"
        nodeToShape (SomeNode (Node SolutionRepr _ _ _ _)) = "circle"
        nodeToShape (SomeNode (Node JustificationRepr _ _ _ _)) = "ellipse"
        nodeToShape (SomeNode (Node AssumptionRepr _ _ _ _)) = "ellipse"
        nodeToShape (SomeNode (Node GoalRepr _ c _ _)) | Just s <- nodePolicyContentToShape c = s
        nodeToShape (SomeNode (Node GoalRepr _ _ _ _)) = "box"
        nodeToShape (SomeNode (Node StrategyRepr _ c _ _)) | Just s <- nodePolicyContentToShape c = s
        nodeToShape (SomeNode (Node StrategyRepr _ _ _ _)) = "parallelogram"

        nodePolicyContentToShape :: NodePolicyContent c -> Maybe Text
        nodePolicyContentToShape NodePolicyAnd = Just "box"
        nodePolicyContentToShape NodePolicyOr = Just "diamond"
        nodePolicyContentToShape (NodePolicyNot _) = Just "triangleDown"
        nodePolicyContentToShape (NodePolicyContent _ _) = Nothing

