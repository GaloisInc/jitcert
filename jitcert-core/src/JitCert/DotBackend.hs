{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module JitCert.DotBackend where

import qualified Data.Graph.Inductive          as FGL
import qualified Data.GraphViz                 as Dot
import qualified Data.GraphViz.Attributes.Complete
                                               as Dot
import qualified Data.GraphViz.Printing        as Dot
import qualified Data.Map.Strict               as Map
import           Data.Monoid                    ( (<>) )
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Text.Lazy                as T
import           System.IO
import           Text.Wrap

import           JitCert.GSN
import           JitCert.GSN.Internal

render :: FilePath -> GSNGraph -> IO ()
render fp = renderGraph fp . gsnGraphToLabeled

gsnGraphToLabeled :: GSNGraph -> FGL.Gr NLabel ELabel
gsnGraphToLabeled = FGL.nmap (someNodeToLNode 0) . gsnGraph

gsnGraphToLabeledWithEnv :: GSNGraph -> GSNEnv -> FGL.Gr NLabel ELabel
gsnGraphToLabeledWithEnv GSNGraph {..} gsnEnv =
  FGL.nmap (someNodeToLNodeWithEnv 0 gsnEnv) gsnGraph

-- mkGraph :: GSN -> FGL.Gr NLabel ELabel
-- mkGraph (GSN nodes edges _) = FGL.mkGraph (mkNodes nodes) (mkEdges edges)

-- mkGraphExpanded :: GSN -> FGL.Gr NLabel ELabel
-- mkGraphExpanded (GSN nodes edges env) = FGL.mkGraph (mkNodesExpanded nodes env) (mkEdges edges)
--
-- Find root nodes
-- Set root environment
-- Recursively descend nodes
--
-- edge :: Map NodeId (NodeId, ...)
-- 
-- graphToExpandedGraph :: GSNGraph -> FGL.Gr NLabel ELabel
-- graphToExpandedGraph gsn@GSN{..} = error "TODO"
--     where
--         popEnv :: Map Int (Map Int SomeContext)
--         popEnv = populateContextEnvironment gsn
--     
--     -- Populate environment for each node (Map Int (Map Int SomeContext)?) by recursively descending from root nodes.
--     -- Label nodes with variables replaced from the env.

appendNodeId :: Int -> T.Text -> T.Text
appendNodeId nid l = T.concat ["[", T.pack . show $ nid, "]: ", l]

-- | Render a GSN node, ignoring context.
someNodeToLNode :: Int -> SomeNode -> NLabel
someNodeToLNode verbosity = someNodeToLNodeWithEnv verbosity $ GSNEnv mempty mempty

-- | Render a GSN node, with a context environment.
someNodeToLNodeWithEnv :: Int -> GSNEnv -> SomeNode -> NLabel
someNodeToLNodeWithEnv verbosity env = toLNode verbosity
 where
  sfToText (SFText     t               ) = t
  sfToText (SFContext  (Node _ _ lab _ _)) = renderContextTR lab
  sfToText (SFProperty p               ) = sToText $ renderProperty p

  renderVariable :: Variable -> T.Text
  renderVariable (Variable v) = v

  renderContextTR :: forall c . ContextContent c -> T.Text
  renderContextTR (ContextNoun v _) =
    let tC = renderContextTypeReference $ Proxy @c
    in  tC <> ", " <> renderVariable v

  renderContextTR (ContextVerb _ _) = error "Cannot reference a context verb." -- JP: Make unreachable?
      -- s

  -- -- | Lookup a context from the environment. 
  -- -- Display the context's value if found.
  -- -- Display variable otherwise.
  -- renderContextVariableOrValue :: Int -> T.Text -> T.Text
  -- renderContextVariableOrValue nid v = case Map.lookup nid env of
  --     Nothing -> renderVariable v
  --     Just (SomeContext c) -> renderContextReference

  renderContextNode :: forall c . NodeId c Context -> ContextContent c -> T.Text
  renderContextNode nid (ContextNoun v _)
    | Just (SomeContext c) <- lookupContextNode nid env
    = -- Just do one level lookup for now.
      renderContext c <> ", " <> renderVariable v

  renderContextNode _ (ContextNoun v (Just d)) = d <> ", " <> renderVariable v

  renderContextNode _ (ContextNoun v Nothing) =
    let tC = renderContextTypeReference $ Proxy @c
    in  tC <> ", " <> renderVariable v

  renderContextNode _ (ContextVerb s _p) =
    let f = Proxy @c
    in  let tC = renderContextTypeReference $ argToProxy f
        in  "Whether " <> tC <> " " <> s

  argToProxy :: Proxy (a -> b) -> Proxy a
  argToProxy Proxy = Proxy

  wrapSentence t = t <> "."

  sToText = T.intercalate " " . map sfToText

  renderSentence :: Sentence -> T.Text
  renderSentence = wrapSentence . sToText

  renderSentenceWithPolicy :: NodePolicy c -> Sentence -> T.Text
  renderSentenceWithPolicy (PolicyForall x xs p) s =
    "Forall "
      <> renderVariable (contextVariable x)
      <> " in "
      <> renderVariable (contextVariable xs)
      <> ", "
      <> renderSentenceWithPolicy p s

  renderSentenceWithPolicy PolicyAnd          s = renderSentence s
  renderSentenceWithPolicy PolicyOr           s = renderSentence s
  renderSentenceWithPolicy (PolicyWhen _ _ _) s = renderSentence s
  renderSentenceWithPolicy (PolicyNot p     ) s = renderSentenceWithPolicy p s

  renderSentenceWithPolicySymbol :: NodePolicy c -> Sentence -> T.Text
  renderSentenceWithPolicySymbol p@(PolicyForall _ _ _) s =
    "∀: " <> renderSentenceWithPolicy p s
  renderSentenceWithPolicySymbol p@(PolicyWhen _ _ _) s =
    "⇒: " <> renderSentenceWithPolicy p s
  renderSentenceWithPolicySymbol p s = renderSentenceWithPolicy p s

  renderNodePolicyContent t (NodePolicyContent p s) =
    Right (t, renderSentenceWithPolicySymbol p s)
  renderNodePolicyContent _ NodePolicyOr      = Left Diamond
  renderNodePolicyContent _ NodePolicyAnd     = Left Square
  renderNodePolicyContent _ (NodePolicyNot _) = Left Triangle

-- TODO: (ADI) Change verbosity to a enum
  toLNode :: Int -> SomeNode -> NLabel
  toLNode verbosity node@(SomeNode (Node repr nid lab _ _)) = (case repr of
    GoalRepr ->
      -- TODO: If rendering in expanded mode, replace variable with value from environment.

      either Left (Right . (fmap addInfo)) $ renderNodePolicyContent Goal lab
    SolutionRepr -> case lab of
      (SolutionContent s) ->
        Right (Solution, addInfo $ wordWrapSqrt $ renderSentence $ renderSolution s)
    StrategyRepr      -> either Left (Right . (fmap addInfo)) $ renderNodePolicyContent Strategy lab
    ContextRepr       -> Right (Context, addInfo $ wrapSentence $ renderContextNode nid lab)
    AssumptionRepr    -> Right (Assumption, addInfo $ renderSentence lab)
    JustificationRepr -> Right (Justification, addInfo $ renderSentence lab)
                                                       )
        where
            addInfo = case verbosity of
                        1 -> appendNodeId $ _sNodeId node
                        _ -> id -- default to level 0


  wordWrapSqrt t =
    let l = floor $ (4 :: Double) * (sqrt $ fromIntegral $ T.length t)
    in  T.fromStrict $ wrapText (WrapSettings False False) l $ T.toStrict t


renderGraph :: FilePath -> FGL.Gr NLabel ELabel -> IO ()
renderGraph fp = renderToDot fp . graphToDot

graphToDot :: FGL.Gr NLabel ELabel -> Dot.DotGraph Int
graphToDot = graphToDotWithHighlight $ GSNHighlight mempty

graphToDotWithHighlight
  :: GSNHighlight -> FGL.Gr NLabel ELabel -> Dot.DotGraph Int
graphToDotWithHighlight highlight = Dot.graphToDot (gsnParams highlight)

gsnParams :: GSNHighlight -> Dot.GraphvizParams Int NLabel ELabel () NLabel
gsnParams highlight = Dot.blankParams { Dot.globalAttributes = []
                                      , Dot.isDirected       = True
                                      , Dot.clusterBy        = Dot.N
                                      , Dot.fmtNode = (nodeAttrs highlight)
                                      , Dot.fmtEdge          = edgeAttrs
                                      }

nodeAttrs :: GSNHighlight -> (Int, NLabel) -> Dot.Attributes
nodeAttrs _highlight (_nid, Left Diamond) =
  [ Dot.Shape Dot.DiamondShape
  , Dot.style Dot.filled
  , Dot.fillColor Dot.Black
  , Dot.Label (Dot.StrLabel "")
  ]
nodeAttrs _highlight (_nid, Left Square) =
  [ Dot.Shape Dot.Square
  , Dot.style Dot.filled
  , Dot.fillColor Dot.Black
  , Dot.Label (Dot.StrLabel "")
  ]
nodeAttrs _highlight (_nid, Left Triangle) =
  [ Dot.Shape Dot.InvTriangle
  , Dot.style Dot.filled
  , Dot.fillColor Dot.Black
  , Dot.Label (Dot.StrLabel "")
  ]
  -- error "TODO: Square and black color"
nodeAttrs highlight (nid, Right (Goal, l)) =
  [ Dot.Label (Dot.StrLabel $ appendNodeId nid l)
  , Dot.Shape Dot.BoxShape]
  <> nodeColorAttrs nid highlight
nodeAttrs highlight (nid, Right (Solution, l)) =
  [ Dot.Label (Dot.StrLabel $ appendNodeId nid l)
  , Dot.Shape Dot.Circle]
  -- , Dot.Margin (Dot.DVal 0)]
    <> nodeColorAttrs nid highlight
nodeAttrs highlight (nid, Right (Strategy, l)) =
  [ Dot.Label (Dot.StrLabel $ appendNodeId nid l)
  , Dot.Shape Dot.Parallelogram]
  <> nodeColorAttrs nid highlight
nodeAttrs highlight (nid, Right (Context, l)) =
  [ Dot.Label (Dot.StrLabel $ appendNodeId nid l)
  , Dot.Shape Dot.BoxShape, Dot.Style [Dot.SItem Dot.Rounded []]]
  <> nodeColorAttrs nid highlight
nodeAttrs highlight (nid, Right (Assumption, l)) =
  [ Dot.Label (Dot.StrLabel $ appendNodeId nid l)
  , Dot.Shape Dot.Ellipse]
  <> nodeColorAttrs nid highlight
nodeAttrs highlight (nid, Right (Justification, l)) =
  [ Dot.Label (Dot.StrLabel $ appendNodeId nid l)
  , Dot.Shape Dot.Ellipse]
  <> nodeColorAttrs nid highlight

nodeColorAttrs :: Int -> GSNHighlight -> Dot.Attributes
nodeColorAttrs i (GSNHighlight m) =
  maybe [] (\c -> [Dot.color c]) $ Map.lookup i m
-- , Dot.bgColor Dot.Tomato
-- , Dot.fillColor Dot.Tomato
-- , Dot.color Dot.Tomato

edgeAttrs :: (Int, Int, ELabel) -> Dot.Attributes
edgeAttrs (_, _, (InContextOf, _)) =
  [ Dot.ArrowHead
      (Dot.AType [(Dot.ArrMod Dot.OpenArrow Dot.BothSides, Dot.Normal)])
  ]
edgeAttrs (_, _, (SupportedBy, _)) =
  [ Dot.ArrowHead
      (Dot.AType [(Dot.ArrMod Dot.FilledArrow Dot.BothSides, Dot.Normal)])
  ]

renderPrintWithEnv :: GSNGraph -> GSNEnv -> IO ()
renderPrintWithEnv g =
  print . Dot.runDotCode . Dot.toDot . graphToDot . gsnGraphToLabeledWithEnv g

renderPrintWithEnvEval :: Map.Map SomeNode Bool -> GSNGraph -> GSNEnv -> IO ()
renderPrintWithEnvEval evalMap g = print . Dot.runDotCode . Dot.toDot . (graphToDotWithHighlight $ evalToHighlight evalMap) . gsnGraphToLabeledWithEnv g

renderPrint :: GSNGraph -> IO ()
renderPrint =
  print . Dot.runDotCode . Dot.toDot . graphToDot . gsnGraphToLabeled

saveWithEnv :: FilePath -> GSNGraph -> GSNEnv -> IO ()
saveWithEnv fp g env = saveWithEnvHighlight fp g env $ GSNHighlight mempty

saveWithEnvHighlight :: FilePath -> GSNGraph -> GSNEnv -> GSNHighlight -> IO ()
saveWithEnvHighlight fp g env highlight = withFile fp WriteMode $ \h ->
  hPrint h
    $ Dot.runDotCode
    $ Dot.toDot
    $ graphToDotWithHighlight highlight
    $ gsnGraphToLabeledWithEnv g env

renderToDot :: (Dot.PrintDot t) => FilePath -> Dot.DotGraph t -> IO ()
renderToDot fp g = withFile fp WriteMode $ \h -> do
  let d = Dot.runDotCode (Dot.toDot g)
  hPrint h d

evalToHighlight :: Map.Map SomeNode Bool -> GSNHighlight
evalToHighlight evalMap =
    let nodeColorMap = Map.map (\x -> if x then Dot.Black else Dot.Red) evalMap in
        GSNHighlight $ Map.mapKeys (\k -> _sNodeId k) nodeColorMap
