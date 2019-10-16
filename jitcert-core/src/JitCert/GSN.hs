{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module JitCert.GSN
    ( -- | Module with functionality to build GSN arguments.
      module JitCert.GSN.Builder
      -- | Module that defines types used in GSN arguments.
    , module JitCert.GSN.Types
    , module JitCert.GSN
    )
where

import qualified Data.Graph.Inductive          as FGL
import qualified Data.Map.Strict               as Map
import qualified Data.Text.Lazy                as T

import           JitCert.DocGenerator
import           JitCert.DocGenerator.Types
import           JitCert.GSN.Builder
import           JitCert.GSN.Internal
import           JitCert.GSN.Types

-- expandContextEnvironment :: GSNGraph -> Map Int (Map Int SomeContext)
-- expandContextEnvironment GSNGraph{..} = foldr (helper mempty) mempty gsnRootNodes
-- 
--     where
-- 
--         -- JP: What do we do when values overlap? Throw error? Internally expand into different nodes?
--         helper :: Map Int SomeContext -> SomeNode -> Map Int (Map Int SomeContext) -> Map Int (Map Int SomeContext)
--         helper m n em = 
--             -- Get node's children nodes.
--             let nid = _sNodeId n in
--             let cns = FGL.suc gsnGraph nid in
-- 
--             -- Split up context nodes.
--             let (cns, ns) = List.partition isContextNode cns in
-- 
--             -- Add context nodes with values to environment. 
--             let m' = foldr (\c m -> case Map.lookup (nodeId c) gsnContextEnvironment of
--                     Nothing -> m -- Context's value not set in environment.
--                     Just v -> Map.insert (nodeId c) m
--                   ) m cns
--             in
-- 
--             error "TODO"


-- helper mempty gsnRootNodes mempty
-- 
--         
-- 
--         helper :: Map Int SomeContext -> [SomeNode] -> Map Int (Map Int SomeContext) -> Map Int (Map Int SomeContext)
--         helper m ns' em =
-- 
-- 
--             -- Fold over nodes, recursively call non-context nodes with updated environment.
--             foldr (helper' m') em ns
-- 
--         
--         

-- | Whether a `SomeNode` is a context node.
isContextNode :: SomeNode -> Bool
isContextNode (SomeNode (Node ContextRepr _ _ _ _)) = True
isContextNode _ = False

-- | Whether a `SomeNode` is a noun context node.
isNounContextNode :: SomeNode -> Bool
isNounContextNode (SomeNode (Node ContextRepr _ (ContextNoun _ _) _ _)) = True
isNounContextNode _ = False

-- | Whether a `SomeNode` is a solution node.
isSolutionNode :: SomeNode -> Bool
isSolutionNode (SomeNode (Node SolutionRepr _ _ _ _)) = True
isSolutionNode _ = False


-- | Nodes introduced at a node by a policy.
introducedSomeNode :: SomeNode -> [SomeNode]
introducedSomeNode (SomeNode n) = SomeNode <$> introducedNode n

-- | Nodes introduced at a node by a policy.
introducedNode :: Node c t -> [Node c Context]
introducedNode (Node GoalRepr          _ pc _ _) = introducedNodePolicyContent pc
introducedNode (Node SolutionRepr      _ _  _ _) = []
introducedNode (Node StrategyRepr      _ pc _ _) = introducedNodePolicyContent pc
introducedNode (Node AssumptionRepr    _ _  _ _) = []
introducedNode (Node JustificationRepr _ _  _ _) = []
introducedNode (Node ContextRepr       _ _  _ _) = []

-- | Nodes introduced by a policy content.
introducedNodePolicyContent :: NodePolicyContent c -> [Node c Context]
introducedNodePolicyContent = introducedPolicy . nodePolicyContentToNodePolicy

-- | Nodes introduced by a policy.
introducedPolicy :: NodePolicy c -> [Node c Context]
introducedPolicy PolicyAnd            = []
introducedPolicy PolicyOr             = []
introducedPolicy (PolicyNot p       ) = introducedPolicy p
introducedPolicy (PolicyForall m _ p) = m : introducedPolicy p
introducedPolicy (PolicyWhen   _ _ p) = introducedPolicy p

-- | Lookup a context node's value in the given environment.
lookupContextNode :: NodeId c Context -> GSNEnv -> Maybe SomeContext
lookupContextNode (NodeId uid) env' = case Map.lookup uid env of
    Nothing                     -> Nothing
    Just (GSNEnvContext c     ) -> Just c
    Just (GSNEnvContextEnv c _) -> Just c
    where env = gsnContextEnv env'


-- | Normalize a GSN graph. 
-- Inserts intermediate nodes when policies go against conventions (strategies imply OR, goals imply AND). 
-- Preserves ids of existing nodes.
-- Pulls out NOT policies to separate nodes.
normalizeGSN :: GSNGraph -> GSNGraph
normalizeGSN GSNGraph {..} =
    let (_, highest) = FGL.nodeRange gsnGraph
    in  let gsnRootNodes' = map (lookupNode gsnGraph) gsnRootNodes
        in  let (_, gsnGraph') =
                        foldr normalize (highest + 1, gsnGraph) gsnRootNodes'
            in  GSNGraph gsnGraph' gsnRootNodes gsnHiddenNodes gsnBoundNodes

  where
    normalize node (available, gsnGraph) =
        -- -- Get node's policy.
        -- let policyContentM = nodePolicyContent node in



        -- -- -- Don't need to normalize
        -- -- if maybe False isPolicyNodeContent policyContentM then
        -- --     (available, gsnGraph)
        -- -- else


        -- case (sNodeLeafPolicy policyContentM, maybe False isPolicyNodeContent policyContentM) of
        --     (Nothing, _) -> 
        --         (available, gsnGraph)
        --     (Just (PolicyNot p), False) ->
        --     -- Just PolicyAnd




        -- Get node's children.
        let nid = _sNodeId node
        in
            let children = getLabeledChildNodes gsnGraph nid
            in

            -- Check for NOT policy node that is not standalone.
                case shouldExpandNOTNode node of
                    Just (notNodeF, newNode) -> splitNode nid
                                                          newNode
                                                          notNodeF
                                                          available
                                                          children
                                                          gsnGraph

                    Nothing ->
                        if canExpandANDNode node
                            && not (all (isDefaultANDNode . fst) children)
                            && moreThanOne children
                        then

                            let
                                andNodeF nid = SomeNode $ Node
                                    GoalRepr
                                    (NodeId nid)
                                    NodePolicyAnd
                                    dgEmpty
                                    Nothing
                            in  splitNode nid
                                          node
                                          andNodeF
                                          available
                                          children
                                          gsnGraph
                        else
                            case
                                ( canExpandORNode node
                                , not (all (isDefaultORNode . fst) children)
                                    && moreThanOne children
                                )
                            of
                                (Just updatedNode, True) ->

                                    let
                                        orNodeF nid = SomeNode $ Node
                                            GoalRepr
                                            (NodeId nid)
                                            NodePolicyOr
                                            dgEmpty
                                            Nothing
                                    in  splitNode nid
                                                  updatedNode
                                                  orNodeF
                                                  available
                                                  children
                                                  gsnGraph

                                _ ->

                                    -- No normalization needed so check children.
                                    let children' = map fst children
                                    in  foldr normalize
                                              (available, gsnGraph)
                                              children'

        -- Check if all non-context children satisfy default policy.
        -- Insert new node.
        -- Update old node's policy.
        -- Insert edge between old node and new node.
        -- Insert new outgoing edges.
        -- Delete old edges.
        -- Recursively check the updated node (or the children).

    -- By default, goal nodes imply AND (ignores context, solution, assumption, and justification nodes).
    isDefaultANDNode :: SomeNode -> Bool
    isDefaultANDNode (SomeNode (Node ContextRepr       _ _ _ _)) = True
    isDefaultANDNode (SomeNode (Node GoalRepr          _ _ _ _)) = True
    isDefaultANDNode (SomeNode (Node StrategyRepr      _ _ _ _)) = False
    isDefaultANDNode (SomeNode (Node SolutionRepr      _ _ _ _)) = True
    isDefaultANDNode (SomeNode (Node AssumptionRepr    _ _ _ _)) = True
    isDefaultANDNode (SomeNode (Node JustificationRepr _ _ _ _)) = True

    -- Is there more than 1 non-context child node?
    moreThanOne children =
        1 < (length $ filter (not . isContextNode . fst) children)

    -- By default, strategy nodes imply OR (ignores context nodes).
    isDefaultORNode :: SomeNode -> Bool
    isDefaultORNode (SomeNode (Node StrategyRepr      _ _ _ _)) = True
    isDefaultORNode (SomeNode (Node GoalRepr          _ _ _ _)) = False
    isDefaultORNode (SomeNode (Node ContextRepr       _ _ _ _)) = True
    isDefaultORNode (SomeNode (Node SolutionRepr      _ _ _ _)) = False
    isDefaultORNode (SomeNode (Node AssumptionRepr    _ _ _ _)) = False
    isDefaultORNode (SomeNode (Node JustificationRepr _ _ _ _)) = False

    -- Split node.
    splitNode nid updatedNode insertNodeF available children gsnGraph =
        -- Delete old node.
        let gsnGraph' = replaceNode nid updatedNode gsnGraph
        in 

        -- Insert new node.
            let gsnGraph'' =
                        FGL.insNode (available, insertNodeF available) gsnGraph'
            in 

                           -- Insert edge between old node and new node.
                let gsnGraph''' = FGL.insEdge
                        (nid, available, (SupportedBy, ("" :: T.Text)))
                        gsnGraph''
                in  -- This should always be SupportedBy.

                                                                        -- Swap outgoing edges.
                    let gsnGraph'''' =
                                swapEdges nid available children gsnGraph'''
                    in 

                                                                                               -- Recursively check the updated node.
                        normalize updatedNode (available + 1, gsnGraph'''')

    replaceNode nid updatedNode gsnGraph =
        -- Get parents.
        let parents = getLabeledParentNodes gsnGraph nid
        in

        -- Delete node.
            let gsnGraph' = FGL.delNode nid gsnGraph
            in

            -- Insert node.
                let gsnGraph'' = FGL.insNode (nid, updatedNode) gsnGraph'
                in 

                -- Insert parent edges.
                    FGL.insEdges
                        (map (\(n, e) -> (_sNodeId n, nid, e)) parents)
                        gsnGraph''

    swapEdges nid' available children gsnGraph = foldr
        (\(node, edge) gsnGraph ->
        -- Skip if child is a context node.
            let nid = _sNodeId node
            in
                if isContextNode node
                    then
                    ---- Insert old edge.
                         FGL.insEdge (nid', nid, edge) gsnGraph
                    else
                    -- -- Delete old edge.
                    -- let gsnGraph' = FGL.delEdge (nid', nid) gsnGraph in
                        let gsnGraph' = gsnGraph
                        in 

                        -- Insert new edge.
                            let gsnGraph'' =
                                    FGL.insEdge (available, nid, edge) gsnGraph'
                            in  gsnGraph''
        )
        gsnGraph
        children

    -- Policy contains NOT.
    shouldExpandNOTNode :: SomeNode -> Maybe (Int -> SomeNode, SomeNode)
    shouldExpandNOTNode (SomeNode (Node
                                   GoalRepr
                                   nid
                                   (NodePolicyContent p s)
                                   dg
                                   r
                                  )
                        )
        = (\(p', p) ->
              let
                  nf nid = SomeNode
                      $ Node GoalRepr (NodeId nid) (NodePolicyNot p') dg r
              in
                  let
                      n = SomeNode
                          $ Node GoalRepr nid (NodePolicyContent p s) dg r
                  in  (nf, n)
          )
            <$> removeNOTNode p
    shouldExpandNOTNode (SomeNode (Node GoalRepr nid (NodePolicyNot p) dg r)) =
        (\(p', p) ->
                let nf nid = SomeNode
                        $ Node GoalRepr (NodeId nid) (NodePolicyNot p') dg r
                in  let n = SomeNode $ Node GoalRepr nid (NodePolicyNot p) dg r
                    in  (nf, n)
            )
            <$> removeNOTNode p



    shouldExpandNOTNode (SomeNode (Node GoalRepr _ _ _ _)) = Nothing

    shouldExpandNOTNode (SomeNode (Node StrategyRepr nid (NodePolicyNot p) dg r))
        = (\(p', p) ->
              let
                  nf nid = SomeNode
                      $ Node GoalRepr (NodeId nid) (NodePolicyNot p') dg r
              in  let n = SomeNode $ Node StrategyRepr nid (NodePolicyNot p) dg r
                  in  (nf, n)
          )
            <$> removeNOTNode p
    shouldExpandNOTNode (SomeNode (Node StrategyRepr nid (NodePolicyContent p s) dg r))
        = (\(p', p) ->
              let
                  nf nid = SomeNode
                      $ Node GoalRepr (NodeId nid) (NodePolicyNot p') dg r
              in
                  let
                      n = SomeNode
                          $ Node StrategyRepr nid (NodePolicyContent p s) dg r
                  in  (nf, n)
          )
            <$> removeNOTNode p
    shouldExpandNOTNode (SomeNode (Node StrategyRepr      _ _ _ _)) = Nothing

    shouldExpandNOTNode (SomeNode (Node SolutionRepr      _ _ _ _)) = Nothing
    shouldExpandNOTNode (SomeNode (Node AssumptionRepr    _ _ _ _)) = Nothing
    shouldExpandNOTNode (SomeNode (Node JustificationRepr _ _ _ _)) = Nothing
    shouldExpandNOTNode (SomeNode (Node ContextRepr       _ _ _ _)) = Nothing

    removeNOTNode :: NodePolicy c -> Maybe (NodePolicy c, NodePolicy c)
    removeNOTNode (PolicyNot p) = Just (p, defaultPolicy)
    removeNOTNode PolicyAnd     = Nothing
    removeNOTNode PolicyOr      = Nothing
    removeNOTNode (PolicyWhen c f p) =
        fmap (PolicyWhen c f) <$> removeNOTNode p
    removeNOTNode (PolicyForall c f p) =
        fmap (PolicyForall c f) <$> removeNOTNode p

    defaultPolicy :: NodePolicy c
    defaultPolicy = PolicyAnd

    -- Is a policy that contains AND.
    -- Returns new node (constructor) and updated node).
    -- canExpandANDNode :: SomeNode -> Maybe (Int -> SomeNode, SomeNode)
    canExpandANDNode :: SomeNode -> Bool
    canExpandANDNode (SomeNode (Node GoalRepr _ NodePolicyAnd _ _)) = False
    canExpandANDNode (SomeNode (Node GoalRepr _ p _ _)) =
        isANDPolicy (nodePolicyContentToNodePolicy p)
    canExpandANDNode (SomeNode (Node StrategyRepr _ NodePolicyAnd _ _)) = False
    canExpandANDNode (SomeNode (Node StrategyRepr _ p _ _)) =
        isANDPolicy (nodePolicyContentToNodePolicy p)
    canExpandANDNode (SomeNode (Node SolutionRepr      _ _ _ _)) = False
    canExpandANDNode (SomeNode (Node AssumptionRepr    _ _ _ _)) = False
    canExpandANDNode (SomeNode (Node JustificationRepr _ _ _ _)) = False
    canExpandANDNode (SomeNode (Node ContextRepr       _ _ _ _)) = False

    isANDPolicy :: NodePolicy c -> Bool
    isANDPolicy PolicyAnd            = True
    isANDPolicy (PolicyNot p)        = isANDPolicy p
    isANDPolicy PolicyOr             = False
    isANDPolicy (PolicyWhen   _ _ p) = isANDPolicy p
    isANDPolicy (PolicyForall _ _ p) = isANDPolicy p

    canExpandORNode :: SomeNode -> Maybe SomeNode
    canExpandORNode (SomeNode (Node GoalRepr nid (NodePolicyContent p s) dg ref)) =
        (\p -> SomeNode (Node GoalRepr nid (NodePolicyContent p s) dg ref))
            <$> replaceORPolicy p
    canExpandORNode (SomeNode (Node GoalRepr nid (NodePolicyNot p) dg ref)) =
        (\p -> SomeNode (Node GoalRepr nid (NodePolicyNot p) dg ref))
            <$> replaceORPolicy p
    canExpandORNode (SomeNode (Node GoalRepr _ _ _ _)) = Nothing
    canExpandORNode (SomeNode (Node StrategyRepr nid (NodePolicyContent p s) dg ref))
        = (\p -> SomeNode (Node StrategyRepr nid (NodePolicyContent p s) dg ref))
            <$> replaceORPolicy p
    canExpandORNode (SomeNode (Node StrategyRepr nid (NodePolicyNot p) dg ref)) =
        (\p -> SomeNode (Node StrategyRepr nid (NodePolicyNot p) dg ref))
            <$> replaceORPolicy p
    canExpandORNode (SomeNode (Node StrategyRepr      _ _ _ _)) = Nothing
    canExpandORNode (SomeNode (Node SolutionRepr      _ _ _ _)) = Nothing
    canExpandORNode (SomeNode (Node AssumptionRepr    _ _ _ _)) = Nothing
    canExpandORNode (SomeNode (Node JustificationRepr _ _ _ _)) = Nothing
    canExpandORNode (SomeNode (Node ContextRepr       _ _ _ _)) = Nothing



    -- Replace OR policy with AND policy.
    replaceORPolicy :: NodePolicy c -> Maybe (NodePolicy c)
    replaceORPolicy PolicyAnd          = Nothing
    replaceORPolicy (PolicyNot p)      = PolicyNot <$> replaceORPolicy p
    replaceORPolicy PolicyOr           = Just PolicyAnd
    replaceORPolicy (PolicyWhen a b p) = PolicyWhen a b <$> replaceORPolicy p
    replaceORPolicy (PolicyForall a b p) =
        PolicyForall a b <$> replaceORPolicy p



-- | Extract a node's document generator.
getSomeDocGen :: SomeNode -> DocGenerator
getSomeDocGen (SomeNode (Node _ _ _ dg _)) = dg

-- | Extract the generated document of a graph as `Text`.
docGenText :: GSNGraph -> GSNEnv -> T.Text
docGenText gr env = docText . dropTypes $ docGenExtract gr env

-- | Extract the generated document of a graph as `[TDoc]`.
docGenExtract :: GSNGraph -> GSNEnv -> [TDoc]
docGenExtract gr _ = concatMap (docGenNodeID gr) (gsnRootNodes gr)

-- | Extract the generated document of the given node id.
docGenNodeID :: GSNGraph -> Int -> [TDoc]
docGenNodeID gr@GSNGraph {..} id =
  let cs = FGL.suc gsnGraph id
      node = lookupNode gsnGraph id
      (DocGenerator dg) = getSomeDocGen node
  in dg (map (docGenNodeID gr) cs)

-- | Convert a `[Doc]` to `Text`.
docText :: [Doc] -> T.Text
docText = T.intercalate " " . map docGen1Text
  where
    docGen1Text :: Doc -> T.Text
    docGen1Text (Paragraph docs)
      = "<p>" <> docText docs <> "<\\p>\n"
    docGen1Text (Heading docs)
      = "<h1>\n" <> docText docs <> "<\\h1>\n"
    docGen1Text (NumList docs)
      = "<ul>\n" <> T.concat (map (\d -> "<li>" <> docGen1Text d <> "<\\li>\n") docs) <> "<\\ul>\n"
    docGen1Text (List docs)
      = "<ol>\n" <> T.concat (map (\d -> "<li>" <> docGen1Text d <> "<\\li>\n") docs) <> "<\\ol>\n"
    docGen1Text (Txt txt) = txt
