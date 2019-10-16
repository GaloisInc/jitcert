module JitCert.Analysis where

import qualified Data.Graph.Inductive          as FGL
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           Data.Monoid                    ( (<>) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import           JitCert.GSN
import           JitCert.GSN.Internal
import           JitCert.Internal
import           JitCert.Query                  ( findNodes )

runChecks :: GSNGraph -> GSNEnv -> Set GSNError
runChecks gsn env = mconcat
    [checkUnbound gsn, checkShadowedValues gsn env, checkUnboundValues gsn env]

-- mergeError :: Either [Text] () -> Either [Text] () -> Either [Text] ()
-- mergeError (Left e1) (Left e2) = Left (e1 <> e2)
-- mergeError e1@(Left _) (Right ()) = e1
-- mergeError (Right ()) e2@(Left _) = e2
-- mergeError (Right ()) (Right ()) = Right ()
-- 
-- mergeErrors :: [Either [Text] ()] -> Either [Text] ()
-- mergeErrors = foldr mergeError $ Right ()

-- | Check that context and solution values assigned in the environment do not overlap (are not shadowed).
checkShadowedValues :: GSNGraph -> GSNEnv -> Set GSNError
checkShadowedValues GSNGraph {..} env =
    -- let rnids = map _sNodeId gsnRootNodes in
                                        checkNodes gsnRootNodes' env Set.empty
    -- Set.map (maybe  WarningShadowedValue . FGL.lab gsnGraph) overlapping


  where
    gsnRootNodes' = map (lookupNode gsnGraph) gsnRootNodes

    checkNodes :: [SomeNode] -> GSNEnv -> Set GSNError -> Set GSNError
    checkNodes rnids env e =

        -- Recursively check each node.
        foldr (\nid e' -> checkNode nid env e') e rnids


    checkOverlap :: [GSNEnv] -> Set Int
    checkOverlap envs = snd $ foldr
        (\env (bound, e) ->
            let ks = Set.fromList $ gsnEnvToKeys env
            in  let overlap = ks `Set.intersection` bound
                in  let bound' = ks `Set.union` bound
                    in  (bound', overlap `Set.union` e)
        )
        (mempty, Set.empty)
        envs

    checkNode :: SomeNode -> GSNEnv -> Set GSNError -> Set GSNError
    checkNode n env e =
        -- Get unmerged environments for node.
        let nid = _sNodeId n
        in
            let envss = lookupNodeEnvironmentsUnmerged nid gsnGraph env
            in

            -- For each combination, check that keys do not overlap with each other or the existing env.
                let es = Set.unions $ map checkOverlap $ map (<> [env]) envss
                in
                    let e' = e `Set.union` Set.map (nodeIdToError n) es
                    in

                    -- Merge environments (in each row and env).
                        let envs =
                                map (\envs -> unionGSNEnvs $ env : envs) envss
                        in

                        -- Get child nodes.
                            let cns = FGL.suc gsnGraph nid
                            in
                                let
                                    (e'', cns') = foldr
                                        (\nid (es, cns) ->
                                            case FGL.lab gsnGraph nid of
                                                Nothing ->
                                                    ( Set.insert
                                                        (ErrorInvalidGraphMissingNode
                                                            n
                                                        )
                                                        es
                                                    , cns
                                                    )
                                                Just n -> (es, n : cns)
                                        )
                                        (e', mempty)
                                        cns
                                in 

                                -- Recursively check each environment.
                                    foldr (checkNodes cns') e'' envs

    nodeIdToError n mid = case FGL.lab gsnGraph mid of
        Nothing -> ErrorInvalidGraphMissingNode n
        Just m  -> WarningShadowedValue m


-- | Checks for unbound errors. 
-- Contexts may only be referenced by a node if it is introduced by the node or an ancestor node.
-- Contexts are introduced at a node when the node has a child context node.
checkUnbound :: GSNGraph -> Set GSNError
checkUnbound GSNGraph {..} = foldr (checkNode Set.empty)
                                   Set.empty
                                   gsnRootNodes'
  where
    gsnRootNodes' = map (lookupNode gsnGraph) gsnRootNodes

    checkNode avail' n e =
        -- Get node's children.
        let nid = _sNodeId n
        in
            case mapM (FGL.lab gsnGraph) (FGL.suc gsnGraph nid) of
                Nothing -> Set.singleton $ ErrorInvalidGraphMissingNode n
                Just cns' ->

                    -- Split up context nodes.
                    let (cns, ns) = List.partition isContextNode cns'
                    in

                    -- Add context nodes (and introduced context nodes) to available set.
                        let ins = introducedSomeNode n
                        in
                            let avail = foldr Set.insert avail' (ins ++ cns)
                            in

                            -- Check that current node only references nodes in available set.
                                let
                                    res =
                                        Set.map (ErrorUnbound n)
                                            $ checkBoundsSomeNode n avail
                                in 

                                -- Recursively check children.
                                    foldr (checkNode avail)
                                          (res `Set.union` e)
                                          ns

    -- mkErr n (Left es) = Left ("Unbound error at node: " <> someNodeIdToNodeDescription n:es) -- TODO: Better node description.
    -- mkErr n (Right ()) = Left ["Unbound error at node: " <> someNodeIdToNodeDescription n] -- TODO: Better node description.

    checkBoundsSomeNode :: SomeNode -> Set SomeNode -> Set SomeNode
    checkBoundsSomeNode n avail =
        let referenced = Set.fromList $ nodesReferencedBy n
        in  referenced `Set.difference` avail

-- | Check if a value is assigned to a node that's not in scope.
-- Checks that all nodes with values assigned are children of the given node.
checkUnboundValues :: GSNGraph -> GSNEnv -> Set GSNError
checkUnboundValues g@GSNGraph {..} env =
    let gsnRootNodes' = map (lookupNode gsnGraph) gsnRootNodes
    in  let rnids = map _sNodeId gsnRootNodes'
        in  let introduced = Set.fromList $ gsnEnvToKeys env
            in  let eids = snd $ checkNodes rnids introduced Set.empty env
                in  Set.map
                        (\nid -> case lookupGraphWithHidden nid g of
                            Just n  -> ErrorUnboundValue n
                            Nothing -> ErrorInvalidGraphMissingNodeId nid
                        )
                        eids

  where
    checkNodes nodes introduced e env@GSNEnv {..} =

        -- Fold over nodes.
        let (found, e') = foldr (checkNode env) (mempty, e) nodes
        in 

        -- Make sure that introduced is a subset of found after visiting all child nodes.
            let e'' = (introduced Set.\\ found) `Set.union` e' in
                                                                                         -- let e'' = foldr (\nid e -> mergeError (mkErr nid) e) e' (introduced Set.\\ found) in
                                                                  (found, e'')

    -- mkErr nid = "Value assigned to node, but node is not in scope: " <> nodeIdToNodeDescription nid gsnGraph

    intersections []      = mempty
    intersections [s    ] = s
    intersections (h : t) = h `Set.intersection` intersections t

    checkNode :: GSNEnv -> Int -> (Set Int, Set Int) -> (Set Int, Set Int)
    checkNode env@GSNEnv {..} nid (found, e) =
        -- Build new environments for current node.
        let envs = lookupNodeEnvironments nid gsnGraph env
        in

        -- Add current node to found set.
            let found' = Set.insert nid found
            in

            -- Add any bound nodes to set.
                let
                    found'' = maybe found' (`Set.insert` found')
                        $ Map.lookup nid gsnBoundNodes
                in

                -- Get child nodes.
                    let cns = FGL.suc gsnGraph nid
                    in

                    -- Recursively check child nodes for each env.
                    -- Needs to be found in all environments to be found at this node. JP: This is the opposite?
                        let
                            (founds, es) = unzip $ map
                                (\env' ->
                                    let introduced =
                                                Set.fromList $ gsnEnvToKeys env'
                                    in 

                                    -- Combine env with env'
                                        let env'' = unionGSNEnv env env'
                                        in  checkNodes cns
                                                       introduced
                                                       Set.empty
                                                       env''
                                )
                                envs
                        in  ( found'' `Set.union` intersections founds
                            , Set.unions (e : es)
                            )

-- Check if any solution nodes have multiple parents.
checkSolutionMultipleParents :: GSNGraph -> Set GSNError
checkSolutionMultipleParents gsnGraph = Set.fromList $ map WarningMultipleParents $ findNodes gsnGraph (\_ n ps _ -> isSolutionNode n && length ps > 1)

-- -- | Check that values have been assigned for all context and solution nodes.
-- -- JP: Should this take into account node policies (like Or or OneOfMany)? For now, it doesn't.
-- checkAllAssigned :: GSNGraph -> GSNEnv -> Either [Text] ()
-- checkAllAssigned GSNGraph{..} env = checkNodes gsnRootNodes env $ Right ()
--     
--     -- Traverse graph, starting at root nodes.
--     -- Merge local environments into existing environments.
--     -- At context and solution nodes, make sure they have values in the environment.
-- 
--     where
--         checkNodes nodes env e = foldr (checkNode env) e nodes
-- 
--         checkNode :: GSNEnv -> SomeNode -> Either [Text] () -> Either [Text] ()
--         checkNode env sn@(SomeNode (Node ContextRepr nid _)) e = case Map.lookup (unNodeId nid) $ gsnContextEnv env of
--             Nothing ->
--                 mergeError (mkError sn) e
--             Just _ -> 
--                 e
--         checkNode env sn@(SomeNode (Node SolutionRepr nid _)) e = case Map.lookup (unNodeId nid) $ gsnSolutionEnv env of
--             Nothing ->
--                 mergeError (mkError sn) e
--             Just _ -> 
--                 e
--         checkNode env node e = 
--             -- Get child nodes.
--             let nid = _sNodeId node in
--             case sequence $ map (FGL.lab gsnGraph) $ FGL.suc gsnGraph nid of
--                 Nothing ->
--                     mergeError (Left ["Error: Could not find a node in graph."]) e
--                 Just cns ->
--                     -- Get environments for current node's scope.
-- 
-- 
--                     error "TODO"
--                     
-- 
-- 
-- 
--         mkError sn = Left ["Node has not been assigned a value: " <> snd (someNodeToLNode sn)]

