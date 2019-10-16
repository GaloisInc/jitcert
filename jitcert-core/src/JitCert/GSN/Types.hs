{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE Trustworthy #-}

module JitCert.GSN.Types (module JitCert.GSN.Types, def) where

import           Data.Default                   ( def )
import           Data.Type.Equality             ( (:~:)(..)
                                                , TestEquality(..)
                                                )
import qualified Data.Graph.Inductive          as FGL
import qualified Data.GraphViz                 as Dot
import           Data.Map.Strict                ( Map, keys )
import           Data.Monoid                    ( (<>) )
import           Data.Proxy                     ( Proxy(..) )
import           Data.String                    ( IsString(..) )
import qualified Data.Text.Lazy                as T
import           Type.Reflection

import           JitCert.DocGenerator.Types

-- | The types of nodes allowed in GSN
data NodeTy
  = Goal
  | Solution
  | Strategy
  | Context
  | Assumption
  | Justification
  deriving Show

data NodeTyRepr :: NodeTy -> * where
  GoalRepr          ::NodeTyRepr Goal
  SolutionRepr      ::NodeTyRepr Solution
  StrategyRepr      ::NodeTyRepr Strategy
  ContextRepr       ::NodeTyRepr Context
  AssumptionRepr    ::NodeTyRepr Assumption
  JustificationRepr ::NodeTyRepr Justification


-- | The types of edges allowed in GSN.
data EdgeTy
  = InContextOf
  | SupportedBy

data EdgeTyRepr :: EdgeTy -> * where
  InContextOfRepr ::EdgeTyRepr InContextOf
  SupportedByRepr ::EdgeTyRepr SupportedBy

-- | A GSN node has a type (as DataKinds phantom type parameter), a unique ID,
-- and some kind of label.
data Node context (t :: NodeTy) :: * where
  Node :: NodeTyRepr t -> NodeId context t -> NodeContent context t -> DocGenerator -> Maybe Reference -> Node context t

_nodeId :: Node context t -> Int
_nodeId = unNodeId . nodeId

nodeId :: Node context t -> NodeId context t
nodeId (Node _ i _ _ _) = i

newtype NodeId context (t :: NodeTy) = NodeId {unNodeId :: Int}
    deriving (Eq, Ord)

-- PartOfSpeech? Noun | Verb | ...
--  Noun a?

-- data ContextContent  = forall c . RenderContext c => 
data ContextContent c =
      -- ContextVerb Sentence c --JP: Maybe description shouldn't be a sentence so that the property is more general? Make it a Text?
      forall n . (c ~ (n -> Bool), RenderContext n, Typeable n) => ContextVerb {
        contextVerbDescription :: T.Text
      , contextVerbPredicate :: c -- JP: Built in closed list of supported functions. ???
      }
    | RenderContext c => ContextNoun {
        contextNounVariable :: Variable
      , contextNounDescription :: Maybe T.Text
      -- , contextValue :: c
      }
--     | ContextNodeLambda (c -> Bool)
--     | ContextNodeList [c]

data SolutionContent e =
      (Typeable e, SolutionType e) => SolutionContent e -- Solution node that takes evidence

--      -- Typeable e => SolutionEvidence {
--      --   solutionDescription :: Sentence
--      -- }
--    -- | SolutionAutomatic {
--    --     solutionDescription :: Sentence
--    --   , solutionAutomatic :: e -> Bool
--    --   }
-- -- JP: | SolutionMonadic (c -> IO Bool) ???

type Reference = T.Text

type family NodeContent (c :: *) (t :: NodeTy) :: * where
  NodeContent c Context    = ContextContent c -- (Variable, Sentence)-- c) -- , Maybe (IsA c)) -- Description? Maybe Sentence
  NodeContent c Goal       = NodePolicyContent c -- (NodePolicy c, Sentence)
  NodeContent c Strategy   = NodePolicyContent c -- (NodePolicy c, Sentence)
  NodeContent c Solution   = SolutionContent c
  NodeContent c Assumption = Sentence
  NodeContent c Justification = Sentence
  -- NodeContent _ _          = T.Text
  -- TODO: Define contents of each node.
  -- Solutions can be automatic and contain lambdas (c -> c -> Bool), or be manual.

data NodePolicy c =
      PolicyAnd
    | PolicyOr
    | PolicyNot (NodePolicy c)
    | PolicyForall (Node c Context) (Node [c] Context) (NodePolicy c)
    | PolicyWhen (Node c Context) (Node (c -> Bool) Context) (NodePolicy c)

isPolicyOr :: NodePolicy c -> Bool
isPolicyOr PolicyOr = True
isPolicyOr _        = False

containsPolicyNot :: NodePolicy c -> Bool
containsPolicyNot (PolicyNot _)        = True
containsPolicyNot PolicyAnd            = False
containsPolicyNot PolicyOr             = False
containsPolicyNot (PolicyWhen   _ _ p) = containsPolicyNot p
containsPolicyNot (PolicyForall _ _ p) = containsPolicyNot p


data NodePolicyContent c =
      NodePolicyAnd
    | NodePolicyOr
    | NodePolicyNot (NodePolicy c)
    | NodePolicyContent (NodePolicy c) Sentence

isPolicyNodeContent :: NodePolicyContent c -> Bool
isPolicyNodeContent NodePolicyAnd           = True
isPolicyNodeContent NodePolicyOr            = True
isPolicyNodeContent (NodePolicyNot _      ) = True
isPolicyNodeContent (NodePolicyContent _ _) = False


nodePolicyContentToNodePolicy :: NodePolicyContent c -> NodePolicy c
nodePolicyContentToNodePolicy (NodePolicyContent p _s) = p
nodePolicyContentToNodePolicy NodePolicyOr             = PolicyOr
nodePolicyContentToNodePolicy NodePolicyAnd            = PolicyAnd
nodePolicyContentToNodePolicy (NodePolicyNot p)        = PolicyNot p

-- data NodeExpr =
--       ExprAnd [NodeExpr]
--     | ExprOr [NodeExpr]
--     -- | ExprForall
--     -- | ExprNOfMany

type Sentence = [SentenceFragment]

data SentenceFragment =
    forall c . RenderContext c => SFContext (Node c Context)
  | SFText T.Text
  | forall p . (Property p) => SFProperty p

data SomeNode = forall context ty. SomeNode (Node context ty)
-- data SomeNode context = forall ty. SomeNode (Node context ty)
-- type SomeNode context = PS.Some (Node context)

instance Eq SomeNode where
  n1 == n2 = _sNodeId n1 == _sNodeId n2

instance Ord SomeNode where
  compare n1 n2 = compare (_sNodeId n1) (_sNodeId n2)

data SomeGoal = forall c . SomeGoal (Node c Goal)

-- | Edges contain a source/destination pair as well as a unique ID and a
-- label.
data Edge c d (t :: EdgeTy) :: * where
  Edge ::EdgeTyRepr t -> SrcDst c d t -> Int -> T.Text -> Edge c d t

-- data SomeEdge c d = forall ety. SomeEdge (Edge c d ety)
data SomeEdge = forall c d ety. SomeEdge (Edge c d ety)


-- type SomeEdge context = PS.Some (Edge context)

-- | Constructors for each possible GSN edge type.  Using DataKinds and GADTs
-- lets us restrict which nodes can be connected, making them correct by
-- construction, structurally speaking.
data SrcDst :: * -> * -> EdgeTy -> * where
  ContGC  ::NodeId c Goal     -> NodeId d Context       -> SrcDst c d InContextOf
  ContGA  ::NodeId c Goal     -> NodeId d Assumption    -> SrcDst c d InContextOf
  ContGJ  ::NodeId c Goal     -> NodeId d Justification -> SrcDst c d InContextOf
  ContSC  ::NodeId c Strategy -> NodeId d Context       -> SrcDst c d InContextOf
  ContSA  ::NodeId c Strategy -> NodeId d Assumption    -> SrcDst c d InContextOf
  ContSJ  ::NodeId c Strategy -> NodeId d Justification -> SrcDst c d InContextOf
  SuppGG  ::NodeId c Goal     -> NodeId d Goal          -> SrcDst c d SupportedBy
  SuppGS  ::NodeId c Goal     -> NodeId d Strategy      -> SrcDst c d SupportedBy
  SuppGSn ::NodeId c Goal     -> NodeId d Solution      -> SrcDst c d SupportedBy
  SuppSG  ::NodeId c Strategy -> NodeId d Goal          -> SrcDst c d SupportedBy

class ValidEdge (n1 :: NodeTy) (n2 :: NodeTy) where
    type NodesToEdgeType n1 n2 :: EdgeTy

    -- JP: Can we make `edgeTypeRepr` without proxies?
    edgeTypeRepr :: Proxy n1 -> Proxy n2 -> EdgeTyRepr (NodesToEdgeType n1 n2)
    edgeConst :: NodeId c n1 -> NodeId d n2 -> SrcDst c d (NodesToEdgeType n1 n2)

instance ValidEdge Goal Context where
    type NodesToEdgeType Goal Context = InContextOf
    edgeTypeRepr Proxy Proxy = InContextOfRepr
    edgeConst = ContGC

instance ValidEdge Goal Assumption where
    type NodesToEdgeType Goal Assumption = InContextOf
    edgeTypeRepr Proxy Proxy = InContextOfRepr
    edgeConst = ContGA

instance ValidEdge Goal Justification where
    type NodesToEdgeType Goal Justification = InContextOf
    edgeTypeRepr Proxy Proxy = InContextOfRepr
    edgeConst = ContGJ

instance ValidEdge Strategy Context where
    type NodesToEdgeType Strategy Context = InContextOf
    edgeTypeRepr Proxy Proxy = InContextOfRepr
    edgeConst = ContSC

instance ValidEdge Strategy Assumption where
    type NodesToEdgeType Strategy Assumption = InContextOf
    edgeTypeRepr Proxy Proxy = InContextOfRepr
    edgeConst = ContSA

instance ValidEdge Strategy Justification where
    type NodesToEdgeType Strategy Justification = InContextOf
    edgeTypeRepr Proxy Proxy = InContextOfRepr
    edgeConst = ContSJ

instance ValidEdge Goal Goal where
    type NodesToEdgeType Goal Goal = SupportedBy
    edgeTypeRepr Proxy Proxy = SupportedByRepr
    edgeConst = SuppGG

instance ValidEdge Goal Strategy where
    type NodesToEdgeType Goal Strategy = SupportedBy
    edgeTypeRepr Proxy Proxy = SupportedByRepr
    edgeConst = SuppGS

instance ValidEdge Goal Solution where
    type NodesToEdgeType Goal Solution = SupportedBy
    edgeTypeRepr Proxy Proxy = SupportedByRepr
    edgeConst = SuppGSn

instance ValidEdge Strategy Goal where
    type NodesToEdgeType Strategy Goal = SupportedBy
    edgeTypeRepr Proxy Proxy = SupportedByRepr
    edgeConst = SuppSG


newtype Variable = Variable T.Text
    deriving (Show)

instance IsString Variable where
    fromString = Variable . fromString

class RenderContext context where
    -- | Render an abstract context when it is referenced by other nodes.
    renderContextTypeReference :: Proxy context -> T.Text

    -- | Render a context when it is displayed in a context node.
    renderContext :: context -> T.Text

    -- -- | Render a context when it is referenced by other nodes.
    -- renderContextReference :: context -> T.Text

    -- renderLabel :: Proxy context -> NodeTyRepr nodety -> NodeContent context nodety -> T.Text
    -- renderLabel :: NodeTyRepr nodety -> NodeContent context nodety -> T.Text
    -- renderLabel :: NodeContent context nodety -> T.Text

data SomeContext = forall c . (RenderContext c, Eq c, Typeable c) => SomeContext c

instance Eq SomeContext where
  (SomeContext a) == (SomeContext b) =
    case testEquality (typeOf a) (typeOf b) of
      Nothing   -> False
      Just Refl -> a == b

instance Show SomeContext where
  show (SomeContext a) = show $ renderContext a

-- Some context that can be used as a key for an environment map.
data ContextKey = forall c . (Ord c, Typeable c) => ContextKey c -- JP: Maybe we should drop this.

instance Eq ContextKey where
  (ContextKey a) == (ContextKey b) = case testEquality (typeOf a) (typeOf b) of
    Nothing   -> False
    Just Refl -> a == b

instance Ord ContextKey where
  (ContextKey a) <= (ContextKey b) = case testEquality ta tb of
    Nothing   -> SomeTypeRep ta <= SomeTypeRep tb
    Just Refl -> a <= b

   where
    ta = typeOf a
    tb = typeOf b

data NodeShape =
      Diamond
    | Square
    | Triangle
    deriving Show

type NLabel = Either NodeShape (NodeTy, T.Text)
type ELabel = (EdgeTy, T.Text)

data GSNGraph = GSNGraph {
      gsnGraph :: FGL.Gr SomeNode ELabel
    , gsnRootNodes :: [Int] -- Root node id's.
    , gsnHiddenNodes :: Map Int SomeNode -- Nodes that are implicitly introduces by policies, but hidden from view.
    , gsnBoundNodes :: Map Int Int -- Node that is bound to the key node.
    }

data GSNEnv =
      -- forall f c . GSNEnv {
      GSNEnv {
        gsnContextEnv  :: (Map Int GSNEnvContext)
      , gsnSolutionEnv :: (Map Int SomeEvidence)
      }
      --   gsnContextEnv  :: (Map (NodeId (f c) Context) (GSNEnvContext c))
      -- , gsnSolutionEnv :: (Map (NodeId c Solution) SomeSolution)
      -- Need invariant that a nodeid only appears at one level in GSNEnv.

-- Map from node id's to their highlight colors.
data GSNHighlight = GSNHighlight {
      unGSNHighlight :: Map Int Dot.X11Color
    }

instance Semigroup GSNHighlight where
    (GSNHighlight a) <> (GSNHighlight b) = GSNHighlight (a <> b)

instance Monoid GSNHighlight where
    mempty = GSNHighlight mempty

instance Show GSNEnv where
    show GSNEnv{..} = "{gsnContextEnv:" ++ show (keys gsnContextEnv) <> ", gsnContextEnv: " <> show (keys gsnSolutionEnv)++"}"
 
instance Show GSNEnvContext where
    show (GSNEnvContext _) = "GSNEnvContext"
    show (GSNEnvContextEnv _ _) = "GSNEnvContextEnv" -- show (Map.keys e)


      -- forall c . Ord c => GSNEnvScopes        (Map (NodeId c Context) (Map c GSNEnv))

-- Contexts can either contain a value, or a map of values to environments.
data GSNEnvContext =
    GSNEnvContext    SomeContext
  | GSNEnvContextEnv SomeContext (Map ContextKey GSNEnv)

-- data GSNEnv = GSNEnv {
--       gsnContextEnv :: Map Int SomeContext
--     , gsnSolutionEnv :: Map Int SomeSolution
--     }

-- Properties.

class Property p where
    -- renderProperty :: p -> T.Text
    renderProperty :: p -> [SentenceFragment]

propertyReferencedNodes :: Property p => p -> [SomeNode]
propertyReferencedNodes p = helper $ renderProperty p

 where
  helper []                   = []
  helper ((SFContext  c) : t) = SomeNode c : helper t
  helper ((SFProperty p) : t) = helper (renderProperty p) <> helper t
  helper ((SFText     _) : t) = helper t


-- Evidence for solutions

-- class Evidence (e :: * -> * -> *) where
-- class Evidence e where
--     type EvidenceReferences
-- 
--     -- validEvidence :: e c p -> c -> Bool
--     validEvidence :: e -> c -> Bool
-- 
--     renderEvidence :: e -> Sentence

class Typeable (SolutionEvidence s) => SolutionType s where
    -- type SolutionReferences (PassesTest t c p) = (t, c)
    type SolutionEvidence s = e | e -> s

    validEvidence :: SolutionEvidence s -> Bool

    renderEvidence :: SolutionEvidence s -> T.Text -- Sentence

    renderSolution :: s -> Sentence

    -- I don't think this is needed.
    -- extract :: s -> Node c Context 

    -- solutionReferences :: s -> [SomeContext]

data SomeStrategy = forall c . SomeStrategy (Node c Strategy)

-- data SomeSolution = forall e c p . (Eq (e c p), Typeable (e c p), Evidence e) => SomeSolution (e c p) --JP: SomeEvidence??
-- data SomeSolution = forall e . (SolutionType e) => SomeSolution e
data SomeEvidence = forall e . (Typeable e, Eq e) => SomeEvidence e

instance Eq SomeEvidence where
  (SomeEvidence a) == (SomeEvidence b) =
    case testEquality (typeOf a) (typeOf b) of
      Nothing   -> False
      Just Refl -> a == b

_sNodeId :: SomeNode -> Int
_sNodeId (SomeNode n) = unNodeId $ nodeId n


-- instance ToJSON GSNGraph where
--     toJSON GSNGraph{..} = Aeson.object [
--           "gsnRootNodes" .= map _sNodeId gsnRootNodes
--         , "gsnGraph" .= gsnGraph
--         ]

-- instance FromJSON GSNGraph where
--     parseJSON (Aeson.Object v) = do
--         root <- v .: "gsnRootNodes"
--         graph <- v .: "gsnGraph"
--         return $ GSNGraph graph root
-- 
--     parseJSON _ = fail "invalid json"

-- instance (ToJSON n, ToJSON e) => ToJSON (FGL.Gr n e) where
--     toJSON g = Aeson.object [
--           "edges" .= FGL.labEdges g
--         , "nodes" .= FGL.labNodes g
--         ]

-- instance (FromJSON n, FromJSON e) => FromJSON (FGL.Gr n e) where
--     parseJSON (Aeson.Object v) = do
--         e <- v .: "edges"
--         n <- v .: "nodes"
--         return $ FGL.mkGraph n e
--     
--     parseJSON _ = fail "invalid json"

-- instance ToJSON SomeNode where
--     toJSON (SomeNode n@(Node GoalRepr _ _)) = toJSON n
--     toJSON (SomeNode n@(Node SolutionRepr _ _)) = toJSON n
--     toJSON (SomeNode n@(Node AssumptionRepr _ _)) = toJSON n
--     toJSON (SomeNode n@(Node JustificationRepr _ _)) = toJSON n
--     toJSON (SomeNode n@(Node StrategyRepr _ _)) = toJSON n
--     toJSON (SomeNode n@(Node ContextRepr _ _)) = toJSON n

-- instance FromJSON SomeNode where
--     parseJSON j@(Aeson.Object v) = do
--         repr <- v .: "repr"
--         case (repr :: T.Text) of
--             "GoalRepr" ->
--                 SomeNode <$> (parseJSON j :: Aeson.Parser (Node c Goal))
--             "SolutionRepr" ->
--                 SomeNode <$> (parseJSON j :: Aeson.Parser (Node c Solution))
--             "AssumptionRepr" ->
--                 SomeNode <$> (parseJSON j :: Aeson.Parser (Node c Assumption))
--             "JustificationRepr" ->
--                 SomeNode <$> (parseJSON j :: Aeson.Parser (Node c Justification))
--             "StrategyRepr" ->
--                 SomeNode <$> (parseJSON j :: Aeson.Parser (Node c Strategy))
--             "ContextRepr" ->
--                 SomeNode <$> (parseJSON j :: Aeson.Parser (Node c Context))


-- instance ToJSON (Node c Solution) where
--     toJSON (Node SolutionRepr nid c) = error "TODO"

-- instance FromJSON (Node c Solution) where
--     parseJSON _ = error "TODO"

-- instance ToJSON (Node c Justification) where
--     toJSON (Node JustificationRepr nid c) = error "TODO"

-- instance FromJSON (Node c Justification) where
--     parseJSON _ = error "TODO"

-- instance ToJSON (Node c Strategy) where
--     toJSON (Node StrategyRepr nid c) = error "TODO"

-- instance FromJSON (Node c Strategy) where
--     parseJSON _ = error "TODO"

-- instance ToJSON (Node c Assumption) where
--     toJSON (Node AssumptionRepr nid c) = error "TODO"

-- instance FromJSON (Node c Assumption) where
--     parseJSON _ = error "TODO"

-- instance ToJSON (Node c Context) where
--     toJSON (Node ContextRepr nid c) = error "TODO"
    -- toJSON (Node ContextRepr nid c) = Aeson.object $ [
    --       "nodeId" .= unNodeId nid
    --     , "repr" .= ("ContextRepr" :: T.Text)
    --     ] <> content c
    --     
    --     where 
    --         content (ContextVerb d _) = [
    --               "content" .= ("verb" :: T.Text)
    --             , "description" .= d
    --             -- , "predicate" .= p
    --             , error "TODO: we can serialize predicate?"
    --             ]

-- instance ToJSON (Node c Goal) where
--     toJSON (Node GoalRepr nid (policy, sentence)) = Aeson.object [
--           "nodeId" .= unNodeId nid
--         , "repr" .= ("GoalRepr" :: T.Text)
--         , "policy" .= policy
--         , "sentence" .= sentence
--         ]

-- instance FromJSON (Node c Goal) where
--     parseJSON (Aeson.Object v) = do
--         nid <- v .: "nodeId"
--         policy <- v .: "policy"
--         sentence <- v .: "sentence"
-- 
--         return $ Node GoalRepr (NodeId nid) (policy, sentence)
--     parseJSON _ = fail "invalid json"

-- instance FromJSON (Node c Context) where
--     parseJSON (Aeson.Object v) = do
--         error "TODO"
--     parseJSON _ = fail "invalid json"

-- instance ToJSON EdgeTy where
--     toJSON _ = error "TODO"
-- instance FromJSON EdgeTy where
--     parseJSON _ = error "TODO"

-- instance ToJSON (NodePolicy c) where
--     toJSON PolicyAnd = Aeson.object [
--           "policy" .= ("and" :: T.Text)
--         ]
--     toJSON PolicyOr = Aeson.object [
--           "policy" .= ("or" :: T.Text)
--         ]
--     toJSON (PolicyForall n ns p) = Aeson.object [
--           "policy" .= ("forall" :: T.Text)
--         , "n" .= n
--         , "ns" .= ns
--         , "subpolicy" .= p
--         ]
--     toJSON (PolicyWhen n nf p) = Aeson.object [
--           "policy" .= ("when" :: T.Text)
--         , "n" .= n
--         , "nf" .= nf
--         , "subpolicy" .= p
--         ]

-- instance FromJSON (NodePolicy c) where
--     parseJSON (Aeson.Object v) = do
--         policy <- v .: "policy"
--         case (policy :: T.Text) of
--             "and" -> return PolicyAnd
--             "or" -> return PolicyOr
--             "forall" -> do
--                 n <- v .: "n"
--                 ns <- v .: "ns"
--                 p <- v .: "subpolicy"
--                 return $ PolicyForall n ns p
--             "when" -> do
--                 n <- v .: "n"
--                 nf <- v .: "nf"
--                 p <- v .: "subpolicy"
--                 return $ PolicyWhen n nf p
--     parseJSON _ = fail "invalid json"


-- instance ToJSON SentenceFragment where
--     toJSON (SFText t) = Aeson.object [
--           "sf" .= ("text" :: T.Text)
--         , "text" .= t
--         ]
--     toJSON (SFProperty p) = Aeson.object [
--           "sf" .= ("property" :: T.Text)
--         , "property" .= p
--         ]
--     toJSON _ = error "TODO"

-- instance FromJSON SentenceFragment where
--     parseJSON _ = error "TODO"

-- GSN errors or warnings.
data GSNError =
      ErrorUnbound SomeNode SomeNode
    | ErrorUnboundValue SomeNode
    | ErrorInvalidGraphMissingNode SomeNode
    | ErrorInvalidGraphMissingNodeId Int
    | WarningShadowedValue SomeNode
    | WarningDirty SomeNode
    | WarningEvaluationFalse SomeNode
    | WarningMultipleParents SomeNode

    deriving (Eq, Ord)

data ErrorLevel = Error | Warning

