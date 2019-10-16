module JitCert.Query where

import qualified Data.Graph.Inductive          as FGL
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Type.Reflection

import           JitCert.GSN
import           JitCert.GSN.Internal
import           JitCert.Internal
import           JitCert.Solution


type ParentNodes = [SomeNode]
type ChildNodes = [SomeNode]
-- | Query that finds a node in a GSN graph that matches a given predicate. 
findNodes
    :: GSNGraph
    -> (GSNGraph -> SomeNode -> ParentNodes -> ChildNodes -> Bool)
    -> [SomeNode]
findNodes g@GSNGraph {..} filter = checkNodes gsnRootNodes' mempty

  where

    gsnRootNodes' = map (lookupNode gsnGraph) gsnRootNodes

    checkNodes nodes acc = foldr checkNode acc nodes

    checkNode node acc =
        -- Get parents.
        let nid = _sNodeId node
        in
            let parents = getParentNodes gsnGraph nid
            in


            -- Get children.
                let children = getChildNodes gsnGraph nid
                in

                -- Run filter, add to acc if match.
                    let
                        acc' = if filter g node parents children
                            then node : acc
                            else acc
                    in 

                    -- Check children.
                        checkNodes children acc'


-- | Query that finds a node in a GSN graph that matches a given predicate. 
findNodesWithEnv
    :: GSNGraph
    -> GSNEnv
    -> (  GSNGraph
       -> GSNEnv
       -> SomeNode
       -> ParentNodes
       -> ChildNodes
       -> Bool
       )
    -> [SomeNode]
findNodesWithEnv g@GSNGraph {..} env filter = checkNodes env
                                                         gsnRootNodes'
                                                         mempty
  where
    gsnRootNodes' = map (lookupNode gsnGraph) gsnRootNodes

    checkNodes env nodes acc = foldr (checkNode env) acc nodes

    checkNode env node acc =
        -- Lookup node's value.
        let nid = _sNodeId node
        in
            case lookupNodeEnvironments nid gsnGraph env of
                [] ->
                    -- Get parents.
                    let parents = getParentNodes gsnGraph nid
                    in

                    -- Get children.
                        let children = getChildNodes gsnGraph nid
                        in

                        -- Run filter, add to acc if match.
                            let
                                acc' = if filter g env node parents children
                                    then node : acc
                                    else acc
                            in 

                            -- Check children.
                                checkNodes env children acc'

                envs ->
                    -- Check the node for each of its values.
                    foldr (\e acc -> checkNode e node acc) acc envs

-- | Find nodes that that are supported by either manual inspection or static analysis.
findManualInspectionOrStaticAnalysis :: GSNGraph -> [SomeNode]
findManualInspectionOrStaticAnalysis = flip findNodes filter
  where
    filter :: GSNGraph -> SomeNode -> ParentNodes -> ChildNodes -> Bool
    filter _ (SomeNode (Node GoalRepr _ c _ _)) _ children
        | isPolicyOr (nodePolicyContentToNodePolicy c) = checkChildren children
    filter _ (SomeNode (Node StrategyRepr _ c _ _)) _ children
        | isPolicyOr (nodePolicyContentToNodePolicy c) = checkChildren children
    filter _ _ _ _ = False

    checkChildren cs = uncurry (||) $ foldr
        (\c (accMI, accSA) -> (isMI c || accMI, isSA c || accSA))
        (False, False)
        cs

    isMI :: SomeNode -> Bool
    isMI (SomeNode (Node SolutionRepr _ (SolutionContent content) _ _)) =
        typeRepTyCon (typeOf content)
            == typeRepTyCon (typeRep @ManualInspection)
    isMI _ = False

    isSA :: SomeNode -> Bool
    isSA (SomeNode (Node SolutionRepr _ (SolutionContent content) _ _)) =
        typeRepTyCon (typeOf content)
            == typeRepTyCon (typeRep @StaticProgramAnalysis)
    isSA _ = False



-- | Query the graph for dirty nodes due to environment changes.
-- References must be acyclic or will diverge. 
envDiff :: GSNGraph -> GSNEnv -> GSNEnv -> Set SomeNode
envDiff g@GSNGraph {..} env0 env1 =
    let diffKeys = envDiff' env0 env1
    in  Set.map
                (\nId -> case FGL.lab gsnGraph nId of
                    Nothing -> case Map.lookup nId gsnHiddenNodes of
                        Nothing -> error "envDiff: malformed graph"
                        Just n  -> n
                    Just n -> n
                )
            $ getDependencies diffKeys diffKeys

  where

    getDependencies keys acc | Set.null keys = acc
    getDependencies keys acc =
        -- Get dependencies of differing value nodes.
        let nodeIds = Set.unions $ Set.toList $ Set.map
                (allReferencedDependencies depGraph)
                keys
        in 

        -- Recursively check context nodes.
            let cNodeIds = Set.filter
                    (\nId -> case FGL.lab gsnGraph nId of
                        Nothing -> False
                        Just n@(SomeNode (Node ContextRepr _ _ _ _)) ->
                            not $ Set.member (_sNodeId n) acc
                        Just _ -> False
                    )
                    nodeIds
            in  let acc' = nodeIds `Set.union` acc
                in  getDependencies cNodeIds acc'

    envDiff' env0 env1 =
        -- Get solution nodes with differing values.
        let sDiffKeys = solutionEnvDiff env0 env1
        in 

        -- Get context nodes with differing values in
            let cDiffKeys = contextEnvDiff env0 env1
            in  Set.union sDiffKeys cDiffKeys

    -- Build a dependency graph.
    depGraph = buildReferencedDependencyGraph g

    -- differenceIntersection keys0 keys1 = 
    --     (keys0 Set.\\ keys1, Set.intersection keys0 keys1, keys1 Set.\\ keys0)

    contextEnvDiff :: GSNEnv -> GSNEnv -> Set Int
    contextEnvDiff env0 env1 =
        let cEnv0 = gsnContextEnv env0
        in
            let cEnv1 = gsnContextEnv env1
            in

            -- Get keys of context environment.
                let keys = Map.keysSet cEnv0 `Set.union` Map.keysSet cEnv1
                in

                    Set.unions $ Set.toList $ Set.map
                        (\k -> case (Map.lookup k cEnv0, Map.lookup k cEnv1) of
                            (Nothing, Nothing) -> Set.empty
                            (Just (GSNEnvContext _), Nothing) ->
                                Set.singleton k
                            (Nothing, Just (GSNEnvContext _)) ->
                                Set.singleton k
                            (Just (GSNEnvContext a), Just (GSNEnvContext b)) ->
                                if a == b then Set.empty else Set.singleton k
                            (Just (GSNEnvContext a), Just (GSNEnvContextEnv b envsB))
                                -> diffNested False k a mempty b envsB

                            (Just (GSNEnvContextEnv a envsA), Just (GSNEnvContext b))
                                -> diffNested True k a envsA b mempty

                            (Just (GSNEnvContextEnv a envsA), Just (GSNEnvContextEnv b envsB))
                                -> diffNested False k a envsA b envsB

                            (Just (GSNEnvContextEnv _ m), Nothing) ->
                                Set.insert k
                                    $ Set.unions
                                    $ map nodesInEnv
                                    $ Map.elems m
                                -- JP: Should we skip in this case since the values are no longer there?
                            (Nothing, Just (GSNEnvContextEnv _ m)) ->
                                Set.insert k
                                    $ Set.unions
                                    $ map nodesInEnv
                                    $ Map.elems m

                    -- (x,y) -> error $ "contextEnvDiff nested TODO" <> show (isJust x) <> show (isJust y) -- <> (show k) <> (show x)
                        )
                        keys

    -- nodesInEnvContext (GSNEnvContext _) = mempty
    -- nodesInEnvContext (GSNEnvContextEnv _ m) = Set.unions $ map nodesInEnv $ Map.elems m

    -- nodesInEnv GSNEnv{..} = Set.unions (Map.keysSet gsnContextEnv : Map.keysSet gsnSolutionEnv : map nodesInEnvContext (Map.elems gsnContextEnv))

    diffNested checkRightEmpty k a envsA b envsB =
        let ed = if a == b then Set.empty else Set.singleton k
        in
            let keys = Map.keysSet envsA `Set.union` Map.keysSet envsB
            in

                let
                    dns = Set.map
                        (\k -> case (Map.lookup k envsA, Map.lookup k envsB) of
                        -- If both maps have environments for the given value, recursively get the diff. 
                            (Just envA, Just envB) -> envDiff' envA envB

                            -- If the left map doesn't have it and the right map does, dirty all the nodes in the right map.
                            (Nothing  , Just envB) -> nodesInEnv envB

                            (Just envA, Nothing  ) -> if checkRightEmpty
                                then
                                                          -- If the left map has it and the right map doesn't, it was expected, so check them.
                                     nodesInEnv envA
                                else
                                                          -- If the left map has it and the right map doesn't, it has been removed, so it doesn't need to be checked.
                                     Set.empty

                            (Nothing, Nothing) ->
                                -- Impossible?
                                Set.empty
                        )
                        keys
                in  Set.unions $ Set.toList $ Set.insert ed dns


    nodesInEnv :: GSNEnv -> Set Int
    nodesInEnv GSNEnv {..} =
        let sNodes = Map.keysSet gsnSolutionEnv
        in
            Map.foldrWithKey
                (\k c ns -> Set.insert k $ case c of
                    GSNEnvContext _ -> ns
                    GSNEnvContextEnv _ envs ->
                        Set.unions $ ns : (map nodesInEnv $ Map.elems envs)
                )
                sNodes
                gsnContextEnv

    solutionEnvDiff env0 env1 =
        let sEnv0 = gsnSolutionEnv env0
        in
            let sEnv1 = gsnSolutionEnv env1
            in

            -- Get keys of solution environment.
                let keys0 = Map.keysSet sEnv0
                in
                    let keys1 = Map.keysSet sEnv1
                    in
                        let keys = Set.union keys0 keys1
                        in

                        -- Get keys whose values differ between the environment.
                            let
                                diffKeys = Set.filter
                                    (\k ->
                                        Map.lookup k sEnv0 /= Map.lookup k sEnv1
                                    )
                                    keys
                            in  diffKeys


            -- let (keysOnly0, keysIntersection, keysOnly1) = differenceIntersection keys0 keys1 in





    -- Forall nodes (keys) in env0 and env1, 
    --  if key is only in env1 and it introduces an environment, add all nodes (and their dependencies) in the environment
    --  if key is only in env0, skip since removed?
    --  if key is only in env1, add all referenced dependencies
    --  if values differ, add all referenced dependencies
    --  




-- | Find nodes where evidence or context values are missing.
findMissingValues :: GSNGraph -> GSNEnv -> [SomeNode]
findMissingValues g env = findNodesWithEnv g env filter

  where
    filter :: GSNGraph -> GSNEnv -> SomeNode -> [SomeNode] -> [SomeNode] -> Bool
    filter _ GSNEnv {..} n@(SomeNode (Node ContextRepr _ _ _ _)) _ _ =
        isNothing $ Map.lookup (_sNodeId n) gsnContextEnv
    filter _ GSNEnv {..} n@(SomeNode (Node SolutionRepr _ _ _ _)) _ _ =
        isNothing $ Map.lookup (_sNodeId n) gsnSolutionEnv
    filter _ _ _ _ _ = False



-- | Lookup all the values of a context node in all nested environments.
lookupContextNodeValues
    :: NodeId c Context -> GSNGraph -> GSNEnv -> [SomeContext]
lookupContextNodeValues target@(NodeId targetId) GSNGraph {..} env =
    -- Recurse down the graph until hitting the target id.
    lookupAtNodes env gsnRootNodes'

  where
    gsnRootNodes' = map (lookupNode gsnGraph) gsnRootNodes

    lookupAtNodes :: GSNEnv -> [SomeNode] -> [SomeContext]
    lookupAtNodes env = concatMap (lookupAtNode env)

    -- When we hit target node, get and return its value from the environment.
    lookupAtNode env node | _sNodeId node == targetId =
        maybeToList $ lookupContextNode target env

    lookupAtNode env node =
        -- Get all environments for the given node.
        let nodeId = _sNodeId node
        in
            let envs = lookupNodeEnvironments nodeId gsnGraph env
            in --JP: Do we need lookupNodeEnvironmentsUnmerged?

            -- Get children.
                let cs'' = getChildNodes gsnGraph nodeId
                in

                -- Get introduced nodes.
                    let intros = introducedReferencesBy node
                    in
                        let
                            cs' =
                                map
                                        ( fromJust
                                        . (`Map.lookup` gsnHiddenNodes)
                                        . fst
                                        )
                                    $ Set.toList intros
                        in  let cs = cs' <> cs''
                            in 

                            -- let envs = map (\env -> Set.foldr (\(m,n) env -> case Map.lookup m $ gsnContextEnv env of
                            --         Nothing -> traceShow (show ("Nothing", n, m)) env
                            --         Just v@(GSNEnvContext _) -> traceShow (show ("Just c", n, m)) $ env {gsnContextEnv = Map.insert n v $ gsnContextEnv env}
                            --         Just v@(GSNEnvContextEnv _ _) -> traceShow (show ("Just ce", n, m)) $ env {gsnContextEnv = Map.insert n v $ gsnContextEnv env}
                            --         
                            --       ) env intros) envs' 
                            -- in

                            -- Recursively check nodes.
                                concatMap (\e -> lookupAtNodes e cs)
                                $ env
                                : envs



    -- let envs = lookupNodeEnvironmentsUnmerged uid env in


    -- maybeToList (lookupContextNode nId env) <> [error "TODO"]

    -- Get values introduced by policies.


-- | Lookup all the values of a solution node in all nested environments.
lookupSolutionNodeValues :: NodeId c Solution -> GSNEnv -> [SomeEvidence]
lookupSolutionNodeValues nId@(NodeId uid) GSNEnv {..} =
    -- Lookup node in env.
    case Map.lookup uid gsnSolutionEnv of
        Just s -> [s]
        Nothing ->
            concatMap
                    (\e -> case e of
                        GSNEnvContext _ -> []
                        GSNEnvContextEnv _ m ->
                            concatMap (lookupSolutionNodeValues nId)
                                $ Map.elems m
                    )
                $ Map.elems gsnContextEnv




