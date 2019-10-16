module JitCert.Internal where

import qualified Data.Graph.Inductive          as FGL
import qualified Data.GraphViz                 as Dot
import           Data.Map.Strict                ( Map )
import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Monoid                    ( (<>) )
import           Data.Ord                       ( Down(..) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T

import           JitCert.DotBackend
import           JitCert.GSN
import           JitCert.GSN.Internal



-- | Get the environments for a given node.
-- Merges the environments for the node's child nodes.
lookupNodeEnvironments :: Int -> FGL.Gr SomeNode ELabel -> GSNEnv -> [GSNEnv]
lookupNodeEnvironments nid gsnGraph =
    map unionGSNEnvs . lookupNodeEnvironmentsUnmerged nid gsnGraph

unionGSNEnv :: GSNEnv -> GSNEnv -> GSNEnv
unionGSNEnv env1 env2 =
    GSNEnv (gsnContextEnv env1 `Map.union` gsnContextEnv env2)
        $           gsnSolutionEnv env1
        `Map.union` gsnSolutionEnv env2

unionGSNEnvs :: [GSNEnv] -> GSNEnv
unionGSNEnvs envs =
    let (cs, ss) = unzip
            $ map (\GSNEnv {..} -> (gsnContextEnv, gsnSolutionEnv)) envs
    in  GSNEnv (Map.unions cs) (Map.unions ss)

gsnEnvToKeys :: GSNEnv -> [Int]
gsnEnvToKeys GSNEnv {..} = Map.keys gsnSolutionEnv <> Map.keys gsnContextEnv


-- | Get the environments for a given node, but doesn't merge them.
lookupNodeEnvironmentsUnmerged
    :: Int -> FGL.Gr SomeNode ELabel -> GSNEnv -> [[GSNEnv]]
lookupNodeEnvironmentsUnmerged nid gsnGraph GSNEnv {..} =

    -- Get child nodes.
    let cns = FGL.suc gsnGraph nid
    in 

    -- Lookup child nodes in environment.
        let cEnvs = mapMaybe
                (\cnid -> case Map.lookup cnid gsnContextEnv of
                    Nothing                     -> Nothing
                    Just (GSNEnvContext _     ) -> Nothing
                    Just (GSNEnvContextEnv _ e) -> Just $ Map.elems e
                )
                cns
        in 

                                                                                                                                            -- Return all combinations of environments.
            sequence cEnvs


nodeIdToNodeDescription :: Int -> FGL.Gr SomeNode e -> Text
nodeIdToNodeDescription nid gsnGraph = case FGL.lab gsnGraph nid of
    Nothing -> T.pack $ show nid
    Just n  -> someNodeIdToNodeDescription n

someNodeIdToNodeDescription :: SomeNode -> Text
someNodeIdToNodeDescription n = either (T.pack . show) snd (someNodeToLNode 0 n)

someNodeIdToNodeDescriptionVerbose :: SomeNode -> Text
someNodeIdToNodeDescriptionVerbose node = T.concat [T.pack nodeType, ":", T.pack nodeText]
    where
        nodeL::NLabel = someNodeToLNode 1 node
        nodeType = either show (show . fst) nodeL
        nodeText = either show (T.unpack . snd) nodeL

-- | Returns the context nodes references by some node.
nodesReferencedBy :: SomeNode -> [SomeNode]
nodesReferencedBy (SomeNode n) = nodesReferencedByNode False n

  where
    nodesReferencedByNode :: Bool -> Node c t -> [SomeNode]
    nodesReferencedByNode _ (Node GoalRepr _ c _ _) =
        nodesReferencedByPolicyContent c
    nodesReferencedByNode _ (Node SolutionRepr _ (SolutionContent sol) _ _) =
        nodesReferencedBySentence $ renderSolution sol
    nodesReferencedByNode _ (Node StrategyRepr _ c _ _) =
        nodesReferencedByPolicyContent c
    nodesReferencedByNode _ (Node AssumptionRepr _ sentence _ _) =
        nodesReferencedBySentence sentence
    nodesReferencedByNode _ (Node JustificationRepr _ sentence _ _) =
        nodesReferencedBySentence sentence
    nodesReferencedByNode True n@(Node ContextRepr _ (ContextVerb _ _) _ _) =
        [SomeNode n]
    nodesReferencedByNode False (Node ContextRepr _ (ContextVerb _ _) _ _) = []
    nodesReferencedByNode True n@(Node ContextRepr _ (ContextNoun _ _) _ _) =
        [SomeNode n]
    nodesReferencedByNode False (Node ContextRepr _ (ContextNoun _ _) _ _) = []

    nodesReferencedByPolicyContent (NodePolicyContent policy sentence) =
        nodesReferencedByPolicy policy <> nodesReferencedBySentence sentence
    nodesReferencedByPolicyContent NodePolicyAnd = mempty
    nodesReferencedByPolicyContent NodePolicyOr  = mempty
    nodesReferencedByPolicyContent (NodePolicyNot p) =
        nodesReferencedByPolicy p

    nodesReferencedByPolicy PolicyAnd     = []
    nodesReferencedByPolicy PolicyOr      = []
    nodesReferencedByPolicy (PolicyNot p) = nodesReferencedByPolicy p
    nodesReferencedByPolicy (PolicyForall m n p) =
        nodesReferencedByNode True m
            <> nodesReferencedByNode True n
            <> nodesReferencedByPolicy p
    nodesReferencedByPolicy (PolicyWhen m n p) =
        nodesReferencedByNode True m
            <> nodesReferencedByNode True n
            <> nodesReferencedByPolicy p

    nodesReferencedBySentence :: Sentence -> [SomeNode]
    nodesReferencedBySentence = concatMap nodesReferencedBySentenceFragment

    nodesReferencedBySentenceFragment (SFContext n) = [SomeNode n]
    nodesReferencedBySentenceFragment (SFText    _) = []
    -- nodesReferencedBySentenceFragment (SFProperty _p) = []
    nodesReferencedBySentenceFragment (SFProperty p) =
        concatMap (\(SomeNode n) -> nodesReferencedByNode True n)
            $ propertyReferencedNodes p

-- Build dependency graph for node references.
type DependencyGraph = Map Int (Set Int)
buildReferencedDependencyGraph :: GSNGraph -> DependencyGraph
buildReferencedDependencyGraph GSNGraph {..} =
    let gsnRootNodes' = map (lookupNode gsnGraph) gsnRootNodes
    in  foldr visitNode Map.empty gsnRootNodes'

  where
    visitNode n m =
        -- Get nodes n references.
        let rns = nodesReferencedBy n
        in

        -- Add edge from rn to n.
            let nId = _sNodeId n
            in
                let
                    m' = foldr
                        (\rn m -> Map.insertWith Set.union
                                                 (_sNodeId rn)
                                                 (Set.singleton nId)
                                                 m
                        )
                        m
                        rns
                in

                -- Get references for nodes introduced by n.
                    let
                        m'' =
                            foldr
                                    (\(m, n) mp -> Map.insertWith
                                        Set.union
                                        n
                                        (Set.singleton m)
                                        mp
                                    )
                                    m'
                                $ introducedReferencesBy n
                    in 

                    -- Visit node's children.
                        let cns = someNodeChildren gsnGraph n
                        in  foldr visitNode m'' cns


    someNodeChildren g n =
        let nId = _sNodeId n
        in  let cns =
                        map
                                (\cnId -> case FGL.lab g cnId of
                                    Nothing -> error "malformed graph" -- JP: probably should propagate this error instead
                                    Just cn -> cn
                                )
                            $ FGL.suc g nId
            in  cns

introducedReferencesBy :: SomeNode -> Set (Int, Int)
introducedReferencesBy (SomeNode n) = introducedReferencesByNode n
  where
    introducedReferencesByNode :: Node c g -> Set (Int, Int)
    introducedReferencesByNode (Node GoalRepr _ c _ _) =
        introducedReferencesByNodePolicyContent c
    introducedReferencesByNode (Node SolutionRepr _ (SolutionContent sol) _ _) =
        introducedReferencesBySentence $ renderSolution sol
    introducedReferencesByNode (Node StrategyRepr _ c _ _) =
        introducedReferencesByNodePolicyContent c
    introducedReferencesByNode (Node AssumptionRepr _ sentence _ _) =
        introducedReferencesBySentence sentence
    introducedReferencesByNode (Node JustificationRepr _ sentence _ _) =
        introducedReferencesBySentence sentence
    introducedReferencesByNode (Node ContextRepr _ _ _ _) = mempty

    introducedReferencesByNodePolicyContent
        :: NodePolicyContent c -> Set (Int, Int)
    introducedReferencesByNodePolicyContent (NodePolicyContent policy sentence)
        = introducedReferencesByPolicy policy
            <> introducedReferencesBySentence sentence
    introducedReferencesByNodePolicyContent NodePolicyAnd = mempty
    introducedReferencesByNodePolicyContent NodePolicyOr  = mempty
    introducedReferencesByNodePolicyContent (NodePolicyNot p) =
        introducedReferencesByPolicy p

    introducedReferencesBySentence _ = mempty

    introducedReferencesByPolicy PolicyAnd = mempty
    introducedReferencesByPolicy PolicyOr  = mempty
    introducedReferencesByPolicy (PolicyForall m n p) =
        Set.singleton (unNodeId (nodeId m), unNodeId (nodeId n))
            `mappend` introducedReferencesByPolicy p
    introducedReferencesByPolicy (PolicyNot p) = introducedReferencesByPolicy p
    introducedReferencesByPolicy (PolicyWhen _m _n p) =
        -- JP: Should we include m and n?
        introducedReferencesByPolicy p


allReferencedDependencies :: DependencyGraph -> Int -> Set Int
allReferencedDependencies m = helper
  where
    helper :: Int -> Set Int
    helper n = case Map.lookup n m of
        Nothing -> Set.empty
        Just s  -> Set.unions $ Set.toList $ Set.insert s $ Set.map helper s


-- Lookup node in graph, including hidden nodes.
lookupGraphWithHidden :: Int -> GSNGraph -> Maybe SomeNode
lookupGraphWithHidden k GSNGraph {..} = case FGL.lab gsnGraph k of
    n@(Just _) -> n
    Nothing    -> Map.lookup k gsnHiddenNodes


gsnErrorToMessage :: GSNError -> T.Text
gsnErrorToMessage (ErrorUnbound n ref) =
    "Error: Reference from node `"
        <> someNodeIdToNodeDescription n
        <> "` to unbound node `"
        <> someNodeIdToNodeDescription ref
        <> "`."
gsnErrorToMessage (ErrorInvalidGraphMissingNode n) =
    "Error: Reference to unknown node at node `"
        <> someNodeIdToNodeDescription n
        <> "`."
gsnErrorToMessage (ErrorInvalidGraphMissingNodeId n) =
    "Error: Reference to unknown node at node `" <> T.pack (show n) <> "`."
gsnErrorToMessage (ErrorUnboundValue n) =
    "Error: Value assigned to node `"
        <> someNodeIdToNodeDescription n
        <> "`, but node is not in scope."
gsnErrorToMessage (WarningShadowedValue n) =
    "Warning: Value is shadowed at node `"
        <> someNodeIdToNodeDescription n
        <> "`."
gsnErrorToMessage (WarningDirty n) =
    "Warning: Node `" <> someNodeIdToNodeDescription n <> "` is dirty."
gsnErrorToMessage (WarningEvaluationFalse n) = 
    "Warning: Node `" <> someNodeIdToNodeDescription n <> "` evaluates to False."
gsnErrorToMessage (WarningMultipleParents n) = 
    "Warning: Node `" <> someNodeIdToNodeDescription n <> "` has multiple parents."

gsnErrorToLevel :: GSNError -> ErrorLevel
gsnErrorToLevel (ErrorUnbound _ _                ) = Error
gsnErrorToLevel (ErrorUnboundValue              _) = Error
gsnErrorToLevel (ErrorInvalidGraphMissingNode   _) = Error
gsnErrorToLevel (ErrorInvalidGraphMissingNodeId _) = Error
gsnErrorToLevel (WarningShadowedValue           _) = Warning
gsnErrorToLevel (WarningDirty                   _) = Warning
gsnErrorToLevel (WarningEvaluationFalse         _) = Warning
gsnErrorToLevel (WarningMultipleParents         _) = Warning


gsnErrorsToHighlight :: [GSNError] -> GSNHighlight
gsnErrorsToHighlight =
    GSNHighlight
        . Map.fromList
        . map (\e -> (gsnErrorToNodeId e, errorToColor e))
        . List.sortOn Down

  where
    errorToColor e = case gsnErrorToLevel e of
        Error   -> Dot.Tomato
        Warning -> Dot.Orange

-- gsnErrorToNode :: GSNError -> SomeNode
gsnErrorToNodeId :: GSNError -> Int
gsnErrorToNodeId (ErrorUnbound n _                ) = _sNodeId n
gsnErrorToNodeId (ErrorUnboundValue              n) = _sNodeId n
gsnErrorToNodeId (ErrorInvalidGraphMissingNode   n) = _sNodeId n
gsnErrorToNodeId (ErrorInvalidGraphMissingNodeId n) = n
gsnErrorToNodeId (WarningShadowedValue           n) = _sNodeId n
gsnErrorToNodeId (WarningDirty                   n) = _sNodeId n
gsnErrorToNodeId (WarningEvaluationFalse         n) = _sNodeId n
gsnErrorToNodeId (WarningMultipleParents         n) = _sNodeId n


